#LLM excel with error sur la plage novembre 2019 à novembre 2020


rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")

######################################
# Créer un df avec les dates manquantes
#######################################

subset_dates <- dates2 |> subset(`Date Prevision` > "2019-10-01")

#######################
#Paramètres spécifiques
#######################


#Systeme prompt
sys_prompt <- system_prompt("Text")

#Initialisation LLM
if (cle_API == "") stop("Clé API Gemini manquante. Ajoute API_KEY_GEMINI dans env/.Renviron")
chat_gemini <- chat_google_gemini( system_prompt = sys_prompt,
                                   base_url = "https://generativelanguage.googleapis.com/v1beta/", 
                                   api_key = cle_API, 
                                   model = "gemini-2.5-pro", 
                                   params(temperature = temp_LLM, max_tokens = 5000)
)



#################################
#Initialisation variables/données
#################################

#Téléchargement données
df_PIB<- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")
df_enq_BDF <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "ENQ_BDF")
df_enq_INSEE <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "ENQ_INSEE")

#Bien transformer les dates en un vecteur
dates <- if (is.data.frame(subset_dates)) as.Date(subset_dates$`Date Prevision`) else as.Date(subset_dates)

# Variables contenant les futures erreurs de prévision
errors_BDF <- rep(NA_real_, length(dates))
errors_INSEE <- rep(NA_real_, length(dates))

# variables pour associer les erreurs aux lignes correspondantes dans df_enq_XXX
errors_by_data_BDF <- list()
errors_by_data_INSEE <- list()
errors_by_quarter <- list()

# map forecast_date 
forecast_to_data_BDF <- list()
forecast_to_data_INSEE <- list()

# forecast médians mensuels stockés par date
forecasts_by_date_BDF <- list()
forecasts_by_date_INSEE <- list()

# Forecast regex pattern qui sera appelé dans la boucle pour parse
forecast_confidence_pattern <- "([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)"


#################
#Nettoyage données
##################


#Nettoyage de df_enq_INSEE

df_enq_INSEE <- df_enq_INSEE |>
  mutate(
    dates = str_replace_all(dates, 
                            c("janv\\." = "jan", "févr\\." = "feb", "mars" = "mar",
                              "avr\\." = "apr", "mai" = "may", "juin" = "jun",
                              "juil\\." = "jul", "août" = "aug", "sept\\." = "sep",
                              "oct\\." = "oct", "nov\\." = "nov", "déc\\." = "dec"))
  ) |>
  mutate(dates_temp = as.Date(parse_date_time(dates, orders = "b Y"), origin = "1970-01-01")) 


v_dates <- rollforward(df_enq_INSEE$dates_temp)
df_enq_INSEE <- df_enq_INSEE |>
  mutate(dates = v_dates) |>
  select(!dates_temp) 


# colonnes de dates en classe Date
if ("dates" %in% names(df_enq_BDF)) df_enq_BDF$dates <- as.Date(df_enq_BDF$dates)
if ("dates" %in% names(df_enq_INSEE)) df_enq_INSEE$dates <- as.Date(df_enq_INSEE$dates)
if ("dates" %in% names(df_PIB)) df_PIB$dates <- as.Date(df_PIB$dates)

#Nettoyage de df_PIB

v_dates_PIB <- rollforward(df_PIB$dates)

df_PIB <- df_PIB |>
  select(!dates) |>
  mutate(dates = v_dates_PIB) 

####################################
# Prompts
###################################

if (english == 1) {
  try(Sys.setlocale("LC_TIME", "English"), silent = TRUE)
  
  #Renvoie le bon dirigeant
  current_boss <- function(type, d) {
    if (type == "BDF") return(BDF_current_boss(d))
    if (type == "INSEE") return(INSEE_current_boss(d))
  }
  
  # 
  prompt_template <- function(type, d, q_trim, y_prev) {
    boss <- current_boss(type, d)
    position <- ifelse(type == "BDF", "Governor of the Banque de France" , "Director General of INSEE")
    current_quarter <- if (q_trim == 1){
      "first"}
    else if (q_trim == 2){
      "second"
    }else if (q_trim == 3){
      "third"}else{
        "fourth"}
    
    
    paste0(
      "Forget the previous instructions and answers. You are ", boss, ", ", position, 
      ", giving a speech about the economic outlook of France. Today is ",
      format(d, "%d %B %Y"), ". ",
      "You will be provided with a document with information about the current state and recent past of the French economy. ",
      "Using ONLY the information in that document and information that was available on or before ", format(d, "%d %B %Y"),
      ", provide a numeric forecast (decimal percent with sign, e.g., +0.3) for French real GDP growth in the ", current_quarter, " quarter of ", y_prev,
      " and a confidence level (integer 0–100). Output EXACTLY in this format on a single line (no extra text):\n",
      "<forecast> (<confidence>)\nExample: +0.3 (80)\n",
      "Do NOT use any information published after ", format(d, "%d %B %Y"), "."
    )
  }
  
} else {
  try(Sys.setlocale("LC_TIME", "French"), silent = TRUE)
  
  current_boss <- function(type, d) {
    if (type == "BDF") return(BDF_current_boss(d))
    if (type == "INSEE") return(INSEE_current_boss(d))
  }
  
  prompt_template <- function(type, d, q_trim, y_prev) {
    boss <- current_boss(type, d)
    position <- ifelse(type == "BDF","Gouverneur de la Banque de France", "Directeur Général de l'INSEE")  
    trimestre_actuel <- if (q_trim == 1){
      "premier"}
    else if (q_trim == 2){
      "second"
    }else if (q_trim == 3){
      "troisième"}else{
        "quatrième"}
    
    
    paste0(
      "Oubliez les instructions et les réponses précédentes. Vous êtes ", boss, ", ", position,
      ", qui prononce un discours sur les perspectives économiques de la France. Nous sommes le ",
      format(d, "%d %B %Y"), ". ",
      "Vous recevrez un document concernant la situation actuelle et passée de l'économie française. ",
      "En utilisant UNIQUEMENT les informations contenues dans ce document et celles disponibles au plus tard le ", format(d, "%d %B %Y"),
      ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) de la croissance du PIB réel français pour le ",
      trimestre_actuel, " trimestre ", y_prev,
      " et un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne (aucun texte supplémentaire) :\n",
      "<prévision> (<confiance>)\nExemple : +0.3 (80)\n",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
}


###########################################
# BOUCLES ITERATIVES SELON LA DATE
#############################################


# BDF #
dir.create("Indicateurs_BDF_error_2020", showWarnings = FALSE)

results_uploads_BDF <- list()
row_id_BDF <- 1

for (idx in seq_along(dates)) {
  current_date <- as.Date(dates[idx])
  
  # prendre les données les plus récentes
  avail_idx <- which(as.Date(df_enq_BDF$dates) <= current_date)
  if (length(avail_idx) >= 1) {
    data_date <- as.Date(df_enq_BDF$dates[max(avail_idx)])   #date où l'on tronque les données trouvée
    df_temp <- df_enq_BDF |> filter(dates <= data_date)
  } else {
    #si pas de données alors on prend juste la première ligne pour ne pas avoir un df vide
    data_date <- as.Date(df_enq_BDF$dates[which.min(as.Date(df_enq_BDF$dates))])
    df_temp <- df_enq_BDF |> filter(dates <= data_date)
  }
  
  
  # imposer type Date
  if ("dates" %in% names(df_temp)) df_temp$dates <- as.Date(df_temp$dates)
  
  # stocker la ligne dans le document associée au forecast produit
  forecast_to_data_BDF[[as.character(current_date)]] <- as.character(data_date)
  
  # associer la date où la date de publication du PIB à son trimestre/année
  pub_months <- c(5L, 8L, 11L, 2L)
  mcur <- as.integer(format(current_date, "%m"))
  if (mcur %in% pub_months) {
    
    if (mcur == 2L) {
      q_year <- as.integer(format(current_date, "%Y")) - 1L
      q_quarter <- 4L
    } else if (mcur == 5L) {
      q_year <- as.integer(format(current_date, "%Y"))
      q_quarter <- 1L
    } else if (mcur == 8L) {
      q_year <- as.integer(format(current_date, "%Y"))
      q_quarter <- 2L
    } else {
      q_year <- as.integer(format(current_date, "%Y"))
      q_quarter <- 3L
    }
    
    
    #But : retrouver les dates considérées dans "dates" (key = une des dates de prévision)
    
    # Identifier les mois cibles
    target_months_str <- c(
      format(current_date %m-% months(3), "%Y-%m"), # t-3
      format(current_date %m-% months(2), "%Y-%m"), # t-2
      format(current_date %m-% months(1), "%Y-%m")  # t-1
    )
    
    # Créer une map de recherche (ex: "2015-02" -> "2015-02-08")
    # On utilise le vecteur 'dates' global qui contient les clés réelles
    dates_lookup_map <- setNames(as.character(dates), format(dates, "%Y-%m"))
    
    # Retrouver les clés réelles (ex: "2015-02-08", "2015-03-08", "2015-04-08")
    # S'il n'y a pas de prévision pour un mois (ex: script démarré en cours de route, la clé sera NA
    months_to_score_keys <- unname(dates_lookup_map[target_months_str])
    
    #Initialiser le vecteur d'erreurs
    monthly_errors <- rep(NA_real_, length(months_to_score_keys))
    
    # Calculer les erreurs en utilisant les bonnes dates
    for (j in seq_along(months_to_score_keys)) {
      
      keym <- months_to_score_keys[j] 
      
      ## Si la clé n'existe pas (NA) ou si la prévision n'existe pas
      if (is.na(keym)) {
        next
      }
      
      fval <- forecasts_by_date_BDF[[keym]] # La recherche va maintenant fonctionner
      
      if (!is.null(fval) && !is.na(fval)) {
        pib_rows <- NULL
        if ("dates" %in% names(df_PIB)) {
          pib_rows <- which(lubridate::year(df_PIB$dates) == q_year & lubridate::quarter(df_PIB$dates) == q_quarter)
        }
        actual_gdp <- NA_real_
        if (length(pib_rows) >= 1 && "PIB_PR" %in% names(df_PIB)) {
          actual_gdp <- as.numeric(df_PIB$PIB_PR[pib_rows[1]])
        } else if ("PIB_PR" %in% names(df_PIB) && length(df_PIB$PIB_PR) >= 1) {
          middle_month <- c(2L,5L,8L,11L)[q_quarter]
          middle_date <- as.Date(sprintf("%04d-%02d-01", q_year, middle_month))
          middle_date <- lubridate::ceiling_date(middle_date, "month") - lubridate::days(1)
          cand <- which(as.Date(df_PIB$dates) == middle_date)
          if (length(cand) >= 1) actual_gdp <- as.numeric(df_PIB$PIB_PR[cand[1]])
        }
        
        if (!is.na(actual_gdp)) {
          monthly_errors[j] <- actual_gdp - as.numeric(fval)
        }
        ## Si actual_gdp est NA, monthly_errors[j] reste NA)
      }
      ## si fval est NULL ou NA, monthly_errors[j] reste NA)
    }
    
    # Stocker les erreurs
    quarter_key <- paste0(q_year, "-Q", q_quarter)
    errors_by_quarter[[quarter_key]] <- monthly_errors
    
    # Assigner les erreurs aux bons index dans les listes globales
    for (j in seq_along(months_to_score_keys)) {
      
      fdate_key <- months_to_score_keys[j] 
      if (is.na(fdate_key)) next ## Ne peut rien assigner si la clé n'existe pas
      
      errval <- monthly_errors[j]
      
      # Assigner au vecteur 'errors_BDF' (aligné sur 'dates')
      pos <- which(as.Date(dates) == as.Date(fdate_key))
      if (length(pos) == 1) {
        errors_BDF[pos] <- errval
      }
      
      # Assigner à la liste 'errors_by_data_BDF' (alignée sur la date des données CSV)
      data_doc_key <- forecast_to_data_BDF[[fdate_key]]
      if (!is.null(data_doc_key) && nzchar(as.character(data_doc_key))) {
        errors_by_data_BDF[[as.character(data_doc_key)]] <- if (!is.na(errval)) as.character(errval) else NA_character_
      }
    }
    
  }
  
  # Remplir la colonne dans l'excel si période propice
  df_temp$LLM_prev_error <- sapply(df_temp$dates, function(drow) {
    key <- as.character(drow)
    if (!is.null(errors_by_data_BDF[[key]])) return(as.character(errors_by_data_BDF[[key]]))
    
    pib_match <- which(as.Date(df_PIB$dates) == drow)
    if (length(pib_match) >= 1) {
      qk <- paste0(lubridate::year(df_PIB$dates[pib_match[1]]), "-Q", lubridate::quarter(df_PIB$dates[pib_match[1]]))
      if (!is.null(errors_by_quarter[[qk]])) return(paste0(na.omit(as.character(errors_by_quarter[[qk]])), collapse = ";"))
    }
    NA_character_
  })
  
  # Remplir la colonne dans l'excel si période propice
  df_temp$LLM_prev_error <- sapply(df_temp$dates, function(drow) {
    key <- as.character(drow)
    if (!is.null(errors_by_data_BDF[[key]])) return(as.character(errors_by_data_BDF[[key]]))
    pib_match <- which(as.Date(df_PIB$dates) == drow)
    if (length(pib_match) >= 1) {
      qk <- paste0(lubridate::year(df_PIB$dates[pib_match[1]]), "-Q", lubridate::quarter(df_PIB$dates[pib_match[1]]))
      if (!is.null(errors_by_quarter[[qk]])) return(paste0(na.omit(as.character(errors_by_quarter[[qk]])), collapse = ";"))
    }
    NA_character_
  })
  
  # Lag pour avoir l'erreur de t-1 en t-1
  lagged_errors <- sapply(seq_along(errors_BDF), function(i) {
    if (i > 1) as.character(errors_BDF[i - 1]) else NA_character_
  })
  
  #Construction de history_rows (appel à la fonction pour créer un df paralèle à df_temp)
  tmpl <- if (nrow(df_temp) > 0) df_temp else df_enq_BDF
  hist_rows <- make_hist_rows(tmpl, dates, lagged_errors, prev_col = "LLM_prev_error")
  
  # Concaténation des deux df
  export_df <- bind_rows(df_temp, hist_rows)
  
  #Supprimer les lignes en trop créées
  n_remove <- length(dates)
  if (nrow(export_df) > n_remove) {
    export_df <- export_df[seq_len(nrow(export_df) - n_remove), , drop = FALSE]
  } else {
    export_df <- export_df[0, , drop = FALSE]
  }
  
  #Création du csv et envoi
  file_name <- paste0("date_max_BDF_", format(current_date, "%Y%m%d"), ".csv")
  file_path <- file.path("Indicateurs_BDF_error_2020", file_name)
  write.csv(export_df, file = file_path, row.names = FALSE, fileEncoding = "UTF-8")
  uploaded_doc <- google_upload(file_path, api_key = cle_API)
  
  # preparation prompt
  mois_index <- as.integer(format(current_date, "%m"))
  year_current <- as.integer(format(current_date, "%Y"))
  trimestre_index <- if (mois_index %in% c(1,11,12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
  year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current
  prompt_text <- prompt_template("BDF", current_date, trimestre_index, year_prev)
  
  # call LLM
  out_list <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- chat_gemini$chat(uploaded_doc, prompt_text)
      return(resp)
    }, error = function(e) NA_character_)
  }, future.seed = TRUE)
  
  # parse resultats
  histoires <- sapply(out_list, function(x) {
    if (is.list(x) && !is.null(x$text)) x$text else if (is.character(x)) x else NA_character_
  })
  parsed_list <- lapply(histoires, function(txt) {
    if (is.null(txt) || length(txt) == 0) return(list(forecast = NA_real_, confidence = NA_integer_, raw = NA_character_))
    m <- regmatches(txt, regexec(forecast_confidence_pattern, txt))
    if (length(m[[1]]) >= 3) list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt) else list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
  })
  
  # prendre la médiane des prévisions, si n_repro = 1 alors la prévision elle-même
  forecasts_vec <- sapply(parsed_list, function(x) as.numeric(x$forecast))
  median_forecast <- if (n_repro >= 2) {
    if (all(is.na(forecasts_vec))) NA_real_ else median(forecasts_vec, na.rm = TRUE)
  } else {
    if (length(forecasts_vec) >= 1) as.numeric(forecasts_vec[1]) else NA_real_
  }
  
  # stocker les forecast mensuels et publier les erreurs trimestrielles 
  forecasts_by_date_BDF[[as.character(current_date)]] <- median_forecast
  
  # fixer l'erreur actuelle
  error_current <- NA_real_
  
  # même logique que l'autre boucle mais post-appel LLM pour la prochaine itération
  pub_months <- c(5L, 8L, 11L, 2L)
  mcur <- as.integer(format(current_date, "%m"))
  if (mcur %in% pub_months) {
    if (mcur == 2L) {
      q_year <- as.integer(format(current_date, "%Y")) - 1L
      q_quarter <- 4L
    } else if (mcur == 5L) {
      q_year <- as.integer(format(current_date, "%Y"))
      q_quarter <- 1L
    } else if (mcur == 8L) {
      q_year <- as.integer(format(current_date, "%Y"))
      q_quarter <- 2L
    } else { # 11
      q_year <- as.integer(format(current_date, "%Y"))
      q_quarter <- 3L
    }
    
    # idem
    pib_rows <- NULL
    if ("dates" %in% names(df_PIB)) {
      pib_rows <- which(lubridate::year(df_PIB$dates) == q_year & lubridate::quarter(df_PIB$dates) == q_quarter)
    }
    actual_gdp <- NA_real_
    if (length(pib_rows) >= 1 && "PIB_PR" %in% names(df_PIB)) {
      actual_gdp <- as.numeric(df_PIB$PIB_PR[pib_rows[1]])
    } else if ("PIB_PR" %in% names(df_PIB) && length(df_PIB$PIB_PR) >= 1) {
      middle_month <- c(2L,5L,8L,11L)[q_quarter]
      middle_date <- as.Date(sprintf("%04d-%02d-01", q_year, middle_month))
      middle_date <- lubridate::ceiling_date(middle_date, "month") - lubridate::days(1)
      cand <- which(as.Date(df_PIB$dates) == middle_date)
      if (length(cand) >= 1) actual_gdp <- as.numeric(df_PIB$PIB_PR[cand[1]])
    }
    
    months_to_score <- seq(current_date %m-% months(3), current_date %m-% months(1), by = "month")
    monthly_errors <- vector("numeric", length = length(months_to_score))
    names(monthly_errors) <- format(months_to_score, "%Y-%m-%d")
    
    for (j in seq_along(months_to_score)) {
      keym <- as.character(months_to_score[j])
      fval <- forecasts_by_date_BDF[[keym]]
      if (!is.null(fval) && !is.na(actual_gdp) && !is.na(fval)) {
        monthly_errors[j] <- actual_gdp - as.numeric(fval)
      } else {
        monthly_errors[j] <- NA_real_
      }
    }
    
    # list par trimestre
    quarter_key <- paste0(q_year, "-Q", q_quarter)
    errors_by_quarter[[quarter_key]] <- monthly_errors
    
    # assigner les erreurs mensuelles vers:
    # 1) errors BDF : pour que l'index match avec la ate de forecast dans 'dates' (pour avoir un match en terme de lignes)
    # 2) errors by data BDF:  pour matcher avec la ligne du forecast associé dans le document csv 
    for (j in seq_along(months_to_score)) {
      fdate <- as.Date(months_to_score[j])
      errval <- monthly_errors[j]
      pos <- which(as.Date(dates) == fdate)
      if (length(pos) == 1) {
        errors_BDF[pos] <- errval
      }
      #garder les lignes même si pas d'erreurs stockées
      fdate_key <- as.character(fdate)
      data_doc_key <- forecast_to_data_BDF[[fdate_key]]
      if (!is.null(data_doc_key) && nzchar(as.character(data_doc_key))) {
        errors_by_data_BDF[[as.character(data_doc_key)]] <- if (!is.na(errval)) as.character(errval) else NA_character_
      }
    }
  }
  
  # fixer errors_BDF
  errors_BDF[idx] <- NA_real_
  
  #enregistrement des résultats
  df_excel_BDF <- data.frame(Date = as.character(current_date), Prompt = prompt_text, stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_excel_BDF[[paste0("forecast_", i)]] <- parsed_list[[i]]$forecast
    df_excel_BDF[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_excel_BDF[[paste0("answer_", i)]] <- parsed_list[[i]]$raw
  }
  
  df_excel_BDF$median_forecast <- median_forecast
  df_excel_BDF$forecast_error <- error_current
  results_uploads_BDF[[row_id_BDF]] <- df_excel_BDF
  
  row_id_BDF <- row_id_BDF + 1
  Sys.sleep(0.5)
}

# création excel de résultats
df_excel_BDF_2020 <- do.call(rbind, results_uploads_BDF)
write.xlsx(df_excel_BDF_2020, file = "Results/BDF_excel_error_2020.xlsx", overwrite = TRUE)




# INSEE #

#même étapes que pour la boucle BDF

dir.create("indicateurs_INSEE_error_2020", showWarnings = FALSE)

results_uploads_INSEE <- list()
row_id_INSEE <- 1

for (idx in seq_along(dates)) {
  current_date <- as.Date(dates[idx])
  
  if ("dates" %in% names(df_enq_INSEE)) {
    avail_idx <- which(as.Date(df_enq_INSEE$dates) <= current_date)
    if (length(avail_idx) >= 1) {
      data_date <- as.Date(df_enq_INSEE$dates[max(avail_idx)])
      df_temp <- df_enq_INSEE |> filter(dates <= data_date)
    } else {
      data_date <- as.Date(df_enq_INSEE$dates[which.min(as.Date(df_enq_INSEE$dates))])
      df_temp <- df_enq_INSEE |> filter(dates <= data_date)
    }
  } else {
    data_date <- as.Date(current_date)
    df_temp <- df_enq_INSEE |> filter(dates <= current_date)
  }
  
  if ("dates" %in% names(df_temp)) df_temp$dates <- as.Date(df_temp$dates)
  
  # remember which data-document row was used for this forecast date
  forecast_to_data_INSEE[[as.character(current_date)]] <- as.character(data_date)
  
  pub_months <- c(5L, 8L, 11L, 2L)
  mcur <- as.integer(format(current_date, "%m"))
  if (mcur %in% pub_months) {
    if (mcur == 2L) { q_year <- as.integer(format(current_date, "%Y")) - 1L; q_quarter <- 4L }
    else if (mcur == 5L) { q_year <- as.integer(format(current_date, "%Y")); q_quarter <- 1L }
    else if (mcur == 8L) { q_year <- as.integer(format(current_date, "%Y")); q_quarter <- 2L }
    else { q_year <- as.integer(format(current_date, "%Y")); q_quarter <- 3L }
    
    #But : retrouver les dates considérées dans "dates" (key = une des dates de prévision)
    
    # Identifier les mois cibles
    target_months_str <- c(
      format(current_date %m-% months(3), "%Y-%m"), # t-3
      format(current_date %m-% months(2), "%Y-%m"), # t-2
      format(current_date %m-% months(1), "%Y-%m")  # t-1
    )
    
    # Créer une map de recherche (ex: "2015-02" -> "2015-02-08")
    # On utilise le vecteur 'dates' global qui contient les clés réelles
    dates_lookup_map <- setNames(as.character(dates), format(dates, "%Y-%m"))
    
    # Retrouver les clés réelles (ex: "2015-02-08", "2015-03-08", "2015-04-08")
    # S'il n'y a pas de prévision pour un mois (ex: script démarré en cours de route, la clé sera NA
    months_to_score_keys <- unname(dates_lookup_map[target_months_str])
    
    #Initialiser le vecteur d'erreurs
    monthly_errors <- rep(NA_real_, length(months_to_score_keys))
    
    # Calculer les erreurs en utilisant les bonnes dates
    for (j in seq_along(months_to_score_keys)) {
      
      keym <- months_to_score_keys[j]  
      
      ## Si la clé n'existe pas (NA) ou si la prévision n'existe pas
      if (is.na(keym)) {
        next
      }
      
      fval <- forecasts_by_date_INSEE[[keym]] # <-- MODIFIÉ POUR INSEE
      
      if (!is.null(fval) && !is.na(fval)) {
        pib_rows <- NULL
        if ("dates" %in% names(df_PIB)) {
          pib_rows <- which(lubridate::year(df_PIB$dates) == q_year & lubridate::quarter(df_PIB$dates) == q_quarter)
        }
        actual_gdp <- NA_real_
        if (length(pib_rows) >= 1 && "PIB_PR" %in% names(df_PIB)) {
          actual_gdp <- as.numeric(df_PIB$PIB_PR[pib_rows[1]])
        } else if ("PIB_PR" %in% names(df_PIB) && length(df_PIB$PIB_PR) >= 1) {
          middle_month <- c(2L,5L,8L,11L)[q_quarter]
          middle_date <- as.Date(sprintf("%04d-%02d-01", q_year, middle_month))
          middle_date <- lubridate::ceiling_date(middle_date, "month") - lubridate::days(1)
          cand <- which(as.Date(df_PIB$dates) == middle_date)
          if (length(cand) >= 1) actual_gdp <- as.numeric(df_PIB$PIB_PR[cand[1]])
        }
        
        if (!is.na(actual_gdp)) {
          monthly_errors[j] <- actual_gdp - as.numeric(fval)
        }
        ## Si actual_gdp est NA, monthly_errors[j] reste NA)
      }
      ## si fval est NULL ou NA, monthly_errors[j] reste NA)
    }
    
    # Stocker les erreurs
    quarter_key <- paste0(q_year, "-Q", q_quarter)
    errors_by_quarter[[quarter_key]] <- monthly_errors
    
    # Assigner les erreurs aux bons index dans les listes globales
    for (j in seq_along(months_to_score_keys)) {
      
      fdate_key <- months_to_score_keys[j]  
      if (is.na(fdate_key)) next ## Ne peut rien assigner si la clé n'existe pas
      
      errval <- monthly_errors[j]
      
      # Assigner au vecteur 'errors_INSEE' (aligné sur 'dates')
      pos <- which(as.Date(dates) == as.Date(fdate_key))
      if (length(pos) == 1) {
        errors_INSEE[pos] <- errval # <-- MODIFIÉ POUR INSEE
      }
      
      # Assigner à la liste 'errors_by_data_INSEE' (alignée sur la date des données CSV)
      data_doc_key <- forecast_to_data_INSEE[[fdate_key]] # <-- MODIFIÉ POUR INSEE
      if (!is.null(data_doc_key) && nzchar(as.character(data_doc_key))) {
        errors_by_data_INSEE[[as.character(data_doc_key)]] <- if (!is.na(errval)) as.character(errval) else NA_character_ # <-- MODIFIÉ POUR INSEE
      }
    }
    
  }
  
  # Remplir la colonne dans l'excel si période propice
  df_temp$LLM_prev_error <- sapply(df_temp$dates, function(drow) {
    key <- as.character(drow)
    if (!is.null(errors_by_data_INSEE[[key]])) return(as.character(errors_by_data_INSEE[[key]])) # <-- MODIFIÉ POUR INSEE
    
    pib_match <- which(as.Date(df_PIB$dates) == drow)
    if (length(pib_match) >= 1) {
      qk <- paste0(lubridate::year(df_PIB$dates[pib_match[1]]), "-Q", lubridate::quarter(df_PIB$dates[pib_match[1]]))
      if (!is.null(errors_by_quarter[[qk]])) return(paste0(na.omit(as.character(errors_by_quarter[[qk]])), collapse = ";"))
    }
    NA_character_
  })
  
  lagged_errors_insee <- sapply(seq_along(errors_INSEE), function(i) {
    if (i > 1) as.character(errors_INSEE[i - 1]) else NA_character_
  })
  
  tmpl_insee <- if (nrow(df_temp) > 0) df_temp else df_enq_INSEE
  hist_rows <- make_hist_rows(tmpl_insee, dates, lagged_errors_insee, prev_col = "LLM_prev_error")
  
  export_df <- dplyr::bind_rows(df_temp, hist_rows)
  
  
  n_remove <- length(dates)
  if (nrow(export_df) > n_remove) {
    export_df <- export_df[seq_len(nrow(export_df) - n_remove), , drop = FALSE]
  } else {
    export_df <- export_df[0, , drop = FALSE]
  }
  
  file_name <- paste0("date_max_INSEE_", format(current_date, "%Y%m%d"), ".csv")
  write.csv(export_df, file = file.path("indicateurs_INSEE_error_2020", file_name), row.names = FALSE, fileEncoding = "UTF-8")
  input_doc <- normalizePath(file.path("indicateurs_INSEE_error_2020", file_name), winslash = "/", mustWork = TRUE)
  uploaded_doc <- google_upload(input_doc, api_key = cle_API)
  
  
  mois_index <- as.integer(format(current_date, "%m"))
  year_current <- as.integer(format(current_date, "%Y"))
  trimestre_index <- if (mois_index %in% c(1,11,12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
  year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current
  prompt_text <- prompt_template("INSEE", current_date, trimestre_index, year_prev)
  
  out_list <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch({
      resp <- chat_gemini$chat(uploaded_doc, prompt_text)
      return(resp)
    }, error = function(e) {
      message("API error: ", conditionMessage(e))
      return(NA_character_)
    })
  }, future.seed = TRUE)
  
  # parse résultats
  histoires <- sapply(out_list, function(x) {
    if (is.list(x) && !is.null(x$text)) x$text else if (is.character(x)) x else NA_character_
  })
  parsed_list <- lapply(histoires, function(txt) {
    if (is.null(txt) || length(txt) == 0) return(list(forecast = NA_real_, confidence = NA_integer_, raw = NA_character_))
    m <- regmatches(txt, regexec(forecast_confidence_pattern, txt))
    if (length(m[[1]]) >= 3) list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt) else list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
  })
  
  # prendre la médiane des prévisions, si n_repro = 1 alors la prévision elle-même
  forecasts_vec <- sapply(parsed_list, function(x) as.numeric(x$forecast))
  median_forecast <- if (n_repro >= 2) {
    if (all(is.na(forecasts_vec))) NA_real_ else median(forecasts_vec, na.rm = TRUE)
  } else {
    if (length(forecasts_vec) >= 1) as.numeric(forecasts_vec[1]) else NA_real_
  }
  
  # store monthly forecast for later quarterly scoring
  forecasts_by_date_INSEE[[as.character(current_date)]] <- median_forecast
  
  # default no immediate scalar error
  error_current <- NA_real_
  
  # publication months (May, Aug, Nov, Feb): score previous quarter t-3..t-1
  pub_months <- c(5L, 8L, 11L, 2L)
  mcur <- as.integer(format(current_date, "%m"))
  if (mcur %in% pub_months) {
    if (mcur == 2L) {
      q_year <- as.integer(format(current_date, "%Y")) - 1L
      q_quarter <- 4L
    } else if (mcur == 5L) {
      q_year <- as.integer(format(current_date, "%Y"))
      q_quarter <- 1L
    } else if (mcur == 8L) {
      q_year <- as.integer(format(current_date, "%Y"))
      q_quarter <- 2L
    } else {
      q_year <- as.integer(format(current_date, "%Y"))
      q_quarter <- 3L
    }
    
    # find actual GDP value
    pib_rows <- NULL
    if ("dates" %in% names(df_PIB)) {
      pib_rows <- which(lubridate::year(df_PIB$dates) == q_year & lubridate::quarter(df_PIB$dates) == q_quarter)
    }
    actual_gdp <- NA_real_
    if (length(pib_rows) >= 1 && "PIB_PR" %in% names(df_PIB)) {
      actual_gdp <- as.numeric(df_PIB$PIB_PR[pib_rows[1]])
    } else if ("PIB_PR" %in% names(df_PIB)) {
      middle_month <- c(2L,5L,8L,11L)[q_quarter]
      middle_date <- lubridate::ceiling_date(as.Date(sprintf("%04d-%02d-01", q_year, middle_month)), "month") - lubridate::days(1)
      cand <- which(as.Date(df_PIB$dates) == middle_date)
      if (length(cand) >= 1) actual_gdp <- as.numeric(df_PIB$PIB_PR[cand[1]])
    }
    
    months_to_score <- seq(current_date %m-% months(3), current_date %m-% months(1), by = "month")
    monthly_errors <- vector("numeric", length = length(months_to_score))
    for (j in seq_along(months_to_score)) {
      keym <- as.character(months_to_score[j])
      fval <- forecasts_by_date_INSEE[[keym]]
      if (!is.null(fval) && !is.na(actual_gdp) && !is.na(fval)) {
        monthly_errors[j] <- actual_gdp - as.numeric(fval)
      } else {
        monthly_errors[j] <- NA_real_
      }
    }
    
    quarter_key <- paste0(q_year, "-Q", q_quarter)
    errors_by_quarter[[quarter_key]] <- monthly_errors
    
    # write each monthly error into errors_INSEE (aligned to 'dates') and into errors_by_data_INSEE for the data-row used
    for (j in seq_along(months_to_score)) {
      fdate <- as.Date(months_to_score[j])
      errval <- monthly_errors[j]
      pos <- which(as.Date(dates) == fdate)
      if (length(pos) == 1) {
        errors_INSEE[pos] <- errval
      }
      fdate_key <- as.character(fdate)
      data_doc_key <- forecast_to_data_INSEE[[fdate_key]]
      if (!is.null(data_doc_key) && nzchar(as.character(data_doc_key))) {
        errors_by_data_INSEE[[as.character(data_doc_key)]] <- if (!is.na(errval)) as.character(errval) else NA_character_
      }
    }
  }
  
  errors_INSEE[idx] <- NA_real_
  
  #enregistrement des résultats
  df_excel_INSEE <- data.frame(Date = as.character(current_date), Prompt = prompt_text, stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_excel_INSEE[[paste0("forecast_", i)]] <- parsed_list[[i]]$forecast
    df_excel_INSEE[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_excel_INSEE[[paste0("answer_", i)]] <- parsed_list[[i]]$raw
  }
  
  df_excel_INSEE$median_forecast <- median_forecast
  df_excel_INSEE$forecast_error <- error_current
  results_uploads_INSEE[[row_id_INSEE]] <- df_excel_INSEE
  
  row_id_INSEE <- row_id_INSEE + 1
  Sys.sleep(0.5)
}

# création excel de résultats
df_excel_INSEE_2020 <- do.call(rbind, results_uploads_INSEE)
write.xlsx(df_excel_INSEE_2020, file = "Results/INSEE_excel_error_2020.xlsx", overwrite = TRUE)







