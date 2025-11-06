# ReadMe
# Prévision récursive de croissance du PIB avec LLM sans document

rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")

#######################################
#Initialisation paramètres spécifiques
#######################################


#Systeme prompt
sys_prompt <- system_prompt("noText")

#Initialisation LLM
if (cle_API == "") stop("Clé API Gemini manquante. Ajoute API_KEY_GEMINI dans env/.Renviron")
chat_gemini <- chat_google_gemini( system_prompt = sys_prompt,
                                   base_url = "https://generativelanguage.googleapis.com/v1beta/", 
                                   api_key = cle_API, 
                                   model = "gemini-2.5-pro", 
                                   params(temperature = temp_LLM, max_tokens = 5000)
)

#####################
# QUESTIONS A POSER
#####################


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
      ", and you are giving a speech about the economic outlook of France. Today is ",
      format(d, "%d %B %Y"), ". ",
      "Using only information that was available on or before ", format(d, "%d %B %Y"),
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
      "En utilisant uniquement les informations disponibles au plus tard le ", format(d, "%d %B %Y"),
      ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) de la croissance du PIB réel français pour le ",
      trimestre_actuel, " trimestre ", y_prev,
      " et un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne (aucun texte supplémentaire) :\n",
      "<prévision> (<confiance>)\nExemple : +0.3 (80)\n",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
}

#################
# BOUCLE PRINCIPALE
##################

#Regex pattern
forecast_confidence_pattern <- "([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)"

# Initialisation des listes de résultats séparées
results_BDF_list <- list()
row_id_BDF <- 1

results_INSEE_list <- list()
row_id_INSEE <- 1

t1 <- Sys.time()

for (dt in as.Date(dates$`Date Prevision`)) {
    current_date <- as.Date(dt)
    mois_index <- as.integer(format(current_date, "%m"))
    year_current <- as.integer(format(current_date, "%Y"))
    trimestre_index <- if (mois_index %in% c(1,11,12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
    year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current
    
    for (type in c("BDF", "INSEE")) {
      
      q_text <- prompt_template(type, current_date, trimestre_index, year_prev)
      
      outs <- future_lapply(seq_len(n_repro), function(i) {
        tryCatch({
          resp <- chat_gemini$chat(q_text)
          return(resp)
        }, error = function(e) {
          message("API error for type ", type, " on date ", current_date, ": ", conditionMessage(e))
          return(NA_character_)
        })
      }, future.seed = TRUE)
      
      textes <- sapply(outs, function(x) {
        if (is.list(x) && !is.null(x$text)) {
          return(x$text)
        } else if (is.character(x)) {
          return(x)
        } else {
          return(NA_character_)
        }
      })
      
      parsed <- lapply(textes, function(txt) {
        if (is.null(txt) || length(txt) == 0) return(list(forecast = NA_real_, confidence = NA_integer_, raw = NA_character_))
        m <- regmatches(txt, regexec(forecast_confidence_pattern, txt)) # Utilisation du pattern défini
        if (length(m[[1]]) >= 3) {
          list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt)
        } else {
          list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
        }
      })
      
      #Df temporaire qui va stocker tous les résultats et les redistribuer à la bonne institution
      df_tmp <- data.frame(
        Date = as.character(current_date),
        Type = type,
        Prompt = q_text,
        stringsAsFactors = FALSE
      )
      
      for (i in seq_len(n_repro)) {
        df_tmp[[paste0("forecast_", i)]]  <- parsed[[i]]$forecast
        df_tmp[[paste0("confidence_", i)]] <- parsed[[i]]$confidence
        df_tmp[[paste0("answer_", i)]]     <- parsed[[i]]$raw
      }
      
      # Condition pour bien matcher le df temp aux lists
      if (type == "BDF") {
        results_BDF_list[[row_id_BDF]] <- df_tmp
        row_id_BDF <- row_id_BDF + 1
      } else if (type == "INSEE") {
        results_INSEE_list[[row_id_INSEE]] <- df_tmp
        row_id_INSEE <- row_id_INSEE + 1
      }
      Sys.sleep(0.5)
    }
  }
  
  # Combinaison des dataframes finaux pour BDF et INSEE
  df_results_BDF <- do.call(rbind, results_BDF_list)
  df_results_INSEE <- do.call(rbind, results_INSEE_list)
  
  # Sauvegarde des deux dataframes séparément
  write.xlsx(df_results_BDF, file = "resultats_BDF.xlsx", sheetName = 'prevision', rowNames = FALSE)
  cat("Les résultats pour la BDF ont été sauvegardés dans le fichier : resultats_BDF.xlsx\n")
  
  write.xlsx(df_results_INSEE, file = "resultats_INSEE.xlsx", sheetName = 'prevision', rowNames = FALSE)
  cat("Les résultats pour l'INSEE ont été sauvegardés dans le fichier : resultats_INSEE.xlsx\n")
  
  t2 <- Sys.time()
  diff(range(t1,t2))
