## FONCTION UTILITAIRES ##


#############
#Multi-usages
############

# get_last_doc : retourne le nom du fichier (ex: "EMC_2_2023") le plus récent disponible par rapport à la date où on se place
get_last_doc <- function(date_prev_df, target_date) {
  # target_date is Date
  candidats <- date_prev_df |>
    filter(date_finale_d <= as.Date(target_date))
  if (nrow(candidats) == 0) {
    warning(paste("Aucun document disponible avant", target_date))
    return(NULL)
  }
  dernier <- candidats |>
    arrange(desc(date_finale_d)) |>
    slice(1) |>
    pull(fichier)
  return(dernier)
}

#Rechercher l'EMC du lendemain de la date de prévision
get_next_doc <- function(cur_day) {
  # target_date is Date of the corresponding EMC
  target_date <- cur_day + 1L
  next_doc <- date_publi_prev |>
    filter(date_finale_d == as.Date(target_date))
  return(next_doc$fichier)
}


# path_from_docname : renvoie chemin complet vers le ou les PDF local/locaux
path_from_docname <- function(doc_name, folder) {
  if (is.null(doc_name)) return(NULL)
  
  # Si vecteur de plusieurs noms de docs et non pas un seul
  full_paths <- sapply(doc_name, function(single_doc_name) {
    if (!grepl("\\.pdf$", single_doc_name, ignore.case = TRUE)) {
      single_doc_name <- paste0(single_doc_name, ".pdf")
    }
    
    path <- file.path(folder, single_doc_name)
    
    if (!file.exists(path)) {
      warning("Fichier introuvable : ", path)
      return(NA_character_) # Retourne NA pour les fichiers introuvables
    }
    
    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  })
  
  # Enlever les fichiers qui n'ont éventuellement pas été trouvés
  full_paths <- full_paths[!is.na(full_paths)]
  
  if (length(full_paths) == 0) {
    return(NULL) # Si aucun fichier trouvable
  }
  
  return(full_paths)
}



#Dirigeant de l'INSEE selon la date
INSEE_current_boss <- function(y_p){
  
  if(y_p <= as.Date("2012-02-21")){
    return("Jean-Philippe Cotis")
  }else if (y_p <= as.Date("2025-05-31")){
    return("Jean-Luc Tavernier")
  }else{
    return("Fabrice Lenglart")
  }
}


#Dirigeant de la BDF selon la date
BDF_current_boss <- function(y_p){
  
  if(y_p <= as.Date("2002-12-31")){
    return("Jean-Claude Trichet")
  }else if (y_p <= as.Date("2015-10-31")){
    return("Christian Noyer")
  }else{
    return("François Villeroy de Galhau")
  }
}


#Concaténer les enquêtes de l'INSEE/BDF en un PDF
merge_pdfs <- function(files, output_path) {
  pdf_combine(input = files, output = output_path)
  return(output_path)
}

###########
# LLM_Text
##########



# Obtenir depuis le dossier les 3 documents : SER, BAT, et EMI en les cherchant par date
get_last_insee_docs_by_type <- function(target_date, doc_type, folder_to_search) {
  
  target_date <- as.Date(target_date)
  
  #Chercher l'enquête du mois précédent (rollback ramène au dernier jour du mois d'avant)
  desired_date <- rollback(target_date)
  
  # Format : AAAA_MM_TYPE.pdf
  pattern <- paste0("^(\\d{4})_(\\d{2})_", doc_type, "\\.pdf$")
  all_files <- list.files(folder_to_search, pattern = pattern, full.names = FALSE)
  
  if (length(all_files) == 0) {
    warning(paste("Aucun fichier", doc_type, "trouvé dans", folder_to_search))
    return(NULL)
  }
  
  # Extraction stricte : 4 chiffres + underscore + 2 chiffres
  file_dates_df <- tibble(
    filename = all_files,
    year  = as.integer(str_extract(all_files, "^\\d{4}")),
    month = as.integer(str_extract(all_files, "(?<=\\d{4}_)\\d{2}"))
  ) |>
    mutate(doc_date = ymd(paste(year, month, "01", sep = "-")))
  
  #On cherche l'enquête du mois d'avant qui, par transformation, sera toujours au 1er du mois 
  doc_possible <- file_dates_df |>
    filter(doc_date <= desired_date)
  
  if (nrow(doc_possible) == 0) {
    warning("Aucun document antérieur à la date cible")
    return(NULL)
  }
  
  most_recent_doc_filename <- doc_possible |>
    arrange(desc(doc_date)) |>
    slice(1) |> 
    pull(filename)
  
  full_path <- path_from_docname(most_recent_doc_filename, folder = folder_to_search) 
  return(full_path)
}





################
#LLM_Text_12
################

# get_last_doc : retourne le nom du fichier (ex: "EMC_2_2023") le plus récent disponible par rapport à la date où on se place
get_last_12_doc <- function(target_date) {

  # target_date is Date
  candidats <- date_publi_prev |>
    filter(date_finale_d <= as.Date(target_date) + 1L)
  
  if (nrow(candidats) == 0) {
    warning(paste("Aucun document disponible avant", target_date))
    return(NULL)
  }
  
  derniers <- candidats |>
    arrange(desc(date_finale_d)) |>
    slice_head(n = 12) |>   #prendre les 12 dernières enquêtes BDF
    pull(fichier)
  
  return(derniers)
}


# Obtenir depuis le dossier les 3 documents : SER, BAT, et EMI en les cherchant par date
get_last_12_insee_docs_by_type <- function(target_date, doc_type, folder_to_search) {
  
  target_date <- as.Date(target_date)
  
  
  #Chercher l'enquête du mois précédent (rollback ramène au dernier jour du mois d'avant)
  desired_date <- rollback(target_date)
  
  # Format : AAAA_MM_TYPE.pdf
  pattern <- paste0("^(\\d{4})_(\\d{2})_", doc_type, "\\.pdf$")
  all_files <- list.files(folder_to_search, pattern = pattern, full.names = FALSE)
  
  if (length(all_files) == 0) {
    warning(paste("Aucun fichier", doc_type, "trouvé dans", folder_to_search))
    return(NULL)
  }
  
  # Extraction stricte : 4 chiffres + underscore + 2 chiffres
  file_dates_df <- tibble(
    filename = all_files,
    year  = as.integer(str_extract(all_files, "^\\d{4}")),
    month = as.integer(str_extract(all_files, "(?<=\\d{4}_)\\d{2}"))
  ) |>
    mutate(doc_date = ymd(paste(year, month, "01", sep = "-")))
  
  doc_possible <- file_dates_df |>
    filter(doc_date < desired_date)
  
  if (nrow(doc_possible) == 0) {
    warning("Aucun document antérieur à la date cible")
    return(NULL)
  }
  
  most_recent_doc_filename <- doc_possible |>
    arrange(desc(doc_date)) |>
    slice_head(n = 12) |> 
    pull(filename)
  
  full_path <- path_from_docname(most_recent_doc_filename, folder = folder_to_search) 
  return(full_path)
}

#######################
#LLM_all_inputs
######################
# Fonction pour transformer un df en un markdown (lisible par ellmer)
df_to_markdown_table <- function(df, title = NULL, max_rows = 50) {
  if (is.null(df) || nrow(df) == 0) {
    if (!is.null(title)) {
      return(paste0("No data available for '", title, "' for this period."))
    } else {
      return("No data available for this period.")
    }
  }
  
  # Vérifier que les dates sont dans un format lisible
  for (col in names(df)) {
    if (inherits(df[[col]], "Date")) {
      df[[col]] <- format(df[[col]], "%Y-%m-%d")
    }
    # Convert any list-columns to character for table rendering
    if (is.list(df[[col]])) {
      df[[col]] <- sapply(df[[col]], function(x) paste(x, collapse = ";"))
    }
  }
  
  # Convertir les colonnes en charactères 
  df <- df |> mutate(across(everything(), as.character))
  
  # On choisit aussi un nombre max de lignes à afficher pour avoir une limite à la taille du prompt
  if (nrow(df) > max_rows) {
    df <- tail(df, max_rows)
    # Add a note about truncation to the title
    if (!is.null(title)) {
      title <- paste0(title, " (showing last ", max_rows, " rows)")
    } else {
      title <- paste0("Data (showing last ", max_rows, " rows)")
    }
    warning(paste0("Dataframe truncated to last ", max_rows, " rows for markdown conversion."))
  }
  
  # Header
  header <- paste(names(df), collapse = " | ")
  separator <- paste(rep("---", ncol(df)), collapse = " | ")
  
  # Lignes
  rows <- apply(df, 1, function(row) paste(row, collapse = " | "))
  
  # Combiner le tout
  table_string <- ""
  if (!is.null(title)) {
    table_string <- paste0("### ", title, "\n\n")
  }
  table_string <- paste0(table_string, header, "\n", separator, "\n", paste(rows, collapse = "\n"))
  return(table_string)
}





##############################
#LLM_excel_with_error
##############################

# Fonction de la même structure que un df (df_temp) avec un historique des erreurs pour pouvoir ensuite les rajouter au df initial
make_hist_rows <- function(template_df, dates_vec, llm_prev_vec, prev_col = "LLM_prev_error") {
  n <- length(dates_vec)
  tmpl <- template_df[1, , drop = FALSE]
  hist_rows <- tmpl[rep(1, n), , drop = FALSE]
  for (col in names(hist_rows)) {
    cls <- class(tmpl[[col]])[1]
    hist_rows[[col]] <- switch(cls,
                               "Date" = as.Date(rep(NA, n)),
                               "POSIXct" = as.POSIXct(rep(NA, n)),
                               "POSIXt" = as.POSIXct(rep(NA, n)),
                               "numeric" = rep(NA_real_, n),
                               "double" = rep(NA_real_, n),
                               "integer" = rep(NA_integer_, n),
                               rep(NA_character_, n)
    )
  }
  hist_rows$dates <- as.Date(dates_vec)
  hist_rows[[prev_col]] <- as.character(llm_prev_vec)
  hist_rows
}

# Matcher la prévision au bon PIB_PR (trouver le PIB qui match le quarter)
find_pib_index <- function(cur_date) {
  cur_date <- as.Date(cur_date)
  if ("dates" %in% names(df_PIB)) vec <- as.Date(df_PIB$dates) 
  else if ("Date" %in% names(df_PIB)) vec <- as.Date(df_PIB$Date)
  else vec <- as.Date(df_PIB[[1]])
  # match date exact
  exact <- which(vec == cur_date)
  if (length(exact) == 1) return(exact)
  # match par trimestre
  q_target <- quarter(cur_date)
  y_target <- year(cur_date)
  qidx <- which(quarter(vec) == q_target & lubridate::year(vec) == y_target)
  if (length(qidx) >= 1) return(qidx[1])
  # match avec le dernier PIB en date si pas dans le trimestre
  prev_idx <- which(vec <= cur_date)
  if (length(prev_idx) >= 1) return(tail(prev_idx, 1))
  # si aucune correspondance alors renvoi le prochain PIB (laisser ça ou faire une erreur ?)
  future_idx <- which(vec > cur_date)
  if (length(future_idx) >= 1) return(future_idx[1])
  integer(0)
}
#####################
# STATS DES
#####################
#Passage en long pour stat des plus simple à rédiger

to_long <- function(df, source_name) { 
  df |>
    pivot_longer(
      cols = matches("^(forecast|confidence)_\\d+$"),
      names_to = c(".value", "rep"),
      names_pattern = "(.*)_(\\d+)$"
    ) |>
    mutate(
      rep = as.integer(rep),
      forecast = as.numeric(forecast),
      confidence = as.integer(confidence),
      source = source_name
    )
}



#############################
# COMP RESULTS
############################

# Fonction pour appareiller la date de prévision au PIB du trimestre adéquate
map_forecast_to_quarter <- function(date_forecast) {
  m <- month(date_forecast)
  y <- year(date_forecast)
  
  if (m %in% c(11, 12, 1)) {
    q <- 4
    y <- ifelse(m == 1, y - 1, y)  
  } else if (m %in% 2:4) {
    q <- 1
  } else if (m %in% 5:7) {
    q <- 2
  } else {
    q <- 3
  }
  return(c(y, q))
}



# Fonction pour calculer les erreurs avec prise en compte du trimestre
compute_errors <- function(df_model, df_obs) {
  df_model2 <- df_model |>
    mutate(Date_forecast = as.Date(Date)) |>
    rowwise() |>
    mutate(tmp = list(map_forecast_to_quarter(Date_forecast))) |>
    mutate(Year = tmp[1], Quarter = tmp[2]) |>
    select(-tmp)
  
  df_model2 |>
    left_join(df_obs, by = c("Year", "Quarter")) |>
    mutate(across(starts_with("forecast_"), 
                  ~ .x - PIB_PR, 
                  .names = "error_{.col}")) 
}
################################
# FONCTION LLM + ECONOMETRIE
#################################


## Fonction utilitaire pour calculer les erreurs
calc_errors <- function(obs, preds) {
  err <- preds - obs
  c(MAE = mean(abs(err)), RMSE = sqrt(mean(err^2)))
}
metrics_list <- list()


#Fonction moyenne arithmétique
weight_avg <- function(pred_1, pred_2) {
  comb_data <- cbind(pred_1, pred_2)
  nowcast <- rowMeans(comb_data)
  return(as.vector(nowcast))
}

#Fonction inverse des erreurs moyennes quadratiques

rolling_inversed_weight <- function(y, pred_1, pred_2, rolling_window) {
  
  # vérifications des inputs
  if (!is.numeric(y)) stop("y doit être un vecteur numérique.")
  if (length(pred_1) != length(pred_2) || length(y) != length(pred_1)) {
    stop("y, pred_1 et pred_2 doivent avoir la même longueur.")
  }
  n <- length(y)
  if (rolling_window >= n) stop("rolling_window doit être plus petite que la taille de l’échantillon.")
  
  # initialisation des vecteurs de sortie (on pose que n_modèle = 2)
  nowcast <- rep(NA_real_, n)
  weights_mat <- matrix(NA_real_, nrow = n - rolling_window + 1,
                        ncol = 2, dimnames = list(NULL, c("w_eco", "w_LLM")))
  row_id <- 1
  
  # boucle rolling
  for (i in rolling_window:n) {
    # période courante : la fenêtre d'apprentissage est basée sur les observations passées
    train_idx <- (i - rolling_window + 1):i
    
    # calcul des erreurs dans la fenêtre courante
    err_eco <- y[train_idx] - pred_1[train_idx]
    err_LLM <- y[train_idx] - pred_2[train_idx]
    
    # MSE de chaque modèle dans la fenêtre
    MSE_eco <- mean(err_eco^2, na.rm = TRUE)
    MSE_LLM <- mean(err_LLM^2, na.rm = TRUE)
    
    # calcul des poids inverses du MSE
    w_raw <- c(1 / MSE_eco, 1 / MSE_LLM)
    w_norm <- w_raw / sum(w_raw)
    
    # combinaison pondérée des prévisions à la date i 
    nowcast[i] <- w_norm[1] * pred_1[i] + w_norm[2] * pred_2[i]
    
    # stocker les poids correspondants
    weights_mat[row_id, ] <- w_norm
    row_id <- row_id + 1
  }
  
  # aligner dimensions finales
  weights_mat <- weights_mat[1:(row_id - 1), , drop = FALSE]
  
  #Garder uniquement les prévisions et enlever les NAs
  nowcast = na.omit(nowcast)
  attr(nowcast, "na.action") <- NULL
  
  return(list(
    nowcast = nowcast,
    weights = weights_mat
  ))
}



#Fonction type Method C of Improved methods of combining forecasts (Granger, Ramanathan)

# Inputs :

#  - y : vecteur numérique des valeurs du PIB
#  - forecasts : matrice ou data.frame n x k (prévisions historiques des k modèles)


# Output :

#      combined : vecteur de longueur n (prévisions combinées )
#      weights  : matrix n_preds x (k + intercept) des coefficients estimés à chaque pas


gr_rolling <- function(y, forecasts, rolling_window) {
  # vérifications des inputs
  if (!is.numeric(y)) stop("y doit être un vecteur numérique.")
  if (!(is.matrix(forecasts) || is.data.frame(forecasts))) stop("forecasts doivent être une matrice ou un df.")
  forecasts <- as.data.frame(forecasts)
  n <- length(y)
  if (n != nrow(forecasts)) stop("y et forecasts doivent avoir le même nombre d'observations")
  if (rolling_window >= n) stop("rolling_window doit être strictement inférieur à la longueur de  y.")
  

  #Définition des modèles soit k = 2 modèles
  f1 = forecasts[,1]
  f2 = forecasts[,2]
  
  # Initialisation sorties
  combined <- rep(NA_real_, n)
  # on stockera poids pour chaque prédiction, une colonne pour l'intercept
  coef_names <-  c("(Intercept)", colnames(forecasts)) 
  max_preds <- n - rolling_window + 1
  weights_mat <- matrix(NA_real_, nrow = max_preds, ncol = length(coef_names),
                        dimnames = list(NULL, coef_names))
  row_id <- 1

  # boucle rolling
  for (i in rolling_window:n) {
    train_idx <- (i - rolling_window +1) : i
    test_idx  <- i 
    
    # construire df d'entraînement et de prévision

    df_train <- data.frame(var_y = y[train_idx], var1 = f1[train_idx], var2 =  f2[train_idx])
    df_prev <- data.frame(var_y = y[test_idx], var1 = f1[test_idx], var2 = f2[test_idx])
    
    #regression
    fit <- lm(var_y ~ var1 + var2, data = df_train)
  


    # prédiction et stockage
    pred_val <- predict(fit, newdata = df_prev)
    
    combined[test_idx] <- pred_val
    # récupérer coefficients des poids 
    coefs <- coef(fit)
    weights_mat[row_id, ] <- coefs
    
    
    row_id <- row_id + 1
  }
  
    #matrice des poids
    weights_mat <- weights_mat[1:(row_id - 1), ]

  forecast_comb = na.omit(combined)
  attr(forecast_comb, "na.action") <- NULL
  
  return(list(
    forecast_comb = forecast_comb,
    weights = weights_mat
  ))
}
#############################################################
# Forecast combination (format ISMA)
###############################################



#  Fonction moyenne arithmétique

simple_avg_month_v2 <- function(models) {
  
  n_models <- length(models)
  n_rows <- nrow(models[[1]])
  
  #Modèles doivent avoir la même taille
  for (k in 1:n_models) {
    if (nrow(models[[k]]) != n_rows) stop(paste("Le modèle", k, "n'a pas le même nombre de lignes."))
  }
  
  # df de sortie
  avg_forecasts <- data.frame(matrix(NA_real_, nrow = n_rows, ncol = 3))
  colnames(avg_forecasts) <- c("Mois_1", "Mois_2", "Mois_3")
  

  for (j in 1:3) {
    #
    preds_mat <- do.call(cbind, lapply(models, function(df) df[, j]))
    
    #Moyenne ligne par ligne
    avg_forecasts[, j] <- rowMeans(preds_mat, na.rm = TRUE)
  }
  
  # Nettoyage pour ne pas voir des NA en bas du tableau qd on affiche
  final_nowcast <- na.omit(avg_forecasts)
  attr(final_nowcast, "na.action") <- NULL
  
  return(final_nowcast)
}


# Fonction inverse des MSE

rolling_inversed_weight_month <- function(y, pred_1, pred_2, rolling_window) {
  
  # Au cas où 
  pred_1 <- as.data.frame(pred_1)
  pred_2 <- as.data.frame(pred_2)
  
  if (!is.numeric(y)) stop("y doit être un vecteur numérique.")
  if (nrow(pred_1) != nrow(pred_2) || length(y) != nrow(pred_1)) {
    stop("y, pred_1 et pred_2 doivent avoir le même nombre de lignes (trimestres).")
  }
  
  n <- length(y)
  if (rolling_window >= n) stop("rolling_window doit être plus petite que la taille de l’échantillon.")
  
  # Initialisation des sorties
  nowcast_df <- data.frame(matrix(NA_real_, nrow = n, ncol = 3))
  colnames(nowcast_df) <- c("Mois_1", "Mois_2", "Mois_3")
  
  # weights devient une liste avec une colonne pour chaque mois
  weights_list <- list()
  
  #boucle sur chaque colonne
  for (j in 1:3) {
    
    #vecteurs pour le mois j
    p1_vec <- pred_1[, j]
    p2_vec <- pred_2[, j]
    
    # Initialisation pour ce mois
    nowcast_vec <- rep(NA_real_, n)
    # Matrice des poids pour ce mois
    weights_mat <- matrix(NA_real_, nrow = n - rolling_window + 1,
                          ncol = 2, dimnames = list(NULL, c("w_eco", "w_LLM")))
    row_id <- 1
    
    # Boucle Rolling sur les valeurs extraites
    for (i in rolling_window:n) {
      train_idx <- (i - rolling_window + 1):i
      
      # Calcul des erreurs et MSE
      err_eco <- y[train_idx] - p1_vec[train_idx]
      err_LLM <- y[train_idx] - p2_vec[train_idx]
      
      MSE_eco <- mean(err_eco^2, na.rm = TRUE)
      MSE_LLM <- mean(err_LLM^2, na.rm = TRUE)
      
      # Weights
      w_raw <- c(1 / MSE_eco, 1 / MSE_LLM)
      # Gestion cas division par zéro si MSE très proche de 0
      if (any(is.infinite(w_raw))) { w_raw[is.infinite(w_raw)] <- 1 }
      
      w_norm <- w_raw / sum(w_raw)
      
      # Prévision finale
      nowcast_vec[i] <- w_norm[1] * p1_vec[i] + w_norm[2] * p2_vec[i]
      
      # Stockage weights
      weights_mat[row_id, ] <- w_norm
      row_id <- row_id + 1
    }
    
    # Stockage des résultats du mois j
    nowcast_df[, j] <- nowcast_vec
    # Nettoyage matrice poids
    weights_mat <- weights_mat[1:(row_id - 1), , drop = FALSE]
    weights_list[[paste0("Mois_", j)]] <- weights_mat
  }
  
  # Nettoyage final
  nowcast_df <- na.omit(nowcast_df)
  attr(nowcast_df, "na.action") <- NULL
  
  return(list(
    nowcast = nowcast_df,
    weights = weights_list
  ))
}

# Fonction Granger-Ramanathan (Adaptée pour 3 colonnes)

gr_rolling_month <- function(y, pred_1, pred_2, rolling_window) {
  
  # Vérifications
  pred_1 <- as.data.frame(pred_1)
  pred_2 <- as.data.frame(pred_2)
  
  if (!is.numeric(y)) stop("y doit être un vecteur numérique.")
  if (nrow(pred_1) != nrow(pred_2) || length(y) != nrow(pred_1)) {
    stop("y, pred_1 et pred_2 doivent avoir le même nombre de lignes.")
  }
  
  n <- length(y)
  if (rolling_window >= n) stop("rolling_window doit être strictement inférieur à la longueur de y.")
  
  # Initialisation sorties
  nowcast_df <- data.frame(matrix(NA_real_, nrow = n, ncol = 3))
  colnames(nowcast_df) <- c("Mois_1", "Mois_2", "Mois_3")
  
  weights_list <- list()
  coef_names <- c("(Intercept)", "w_1", "w_2")
  
  #boucle sur chaque colonne
  for (j in 1:3) {
    
    #vecteurs du mois j
    p1_vec <- pred_1[, j]
    p2_vec <- pred_2[, j]
    
    nowcast_vec <- rep(NA_real_, n)
    
    # Matrice poids pour mois j
    max_preds <- n - rolling_window + 1
    weights_mat <- matrix(NA_real_, nrow = max_preds, ncol = length(coef_names),
                          dimnames = list(NULL, coef_names))
    row_id <- 1
    
    # Boucle Rolling
    for (i in rolling_window:n) {
      train_idx <- (i - rolling_window + 1):i
      
      # ERRREUR ICI ? : ON ENTRAINE JUSQUA LA DATE OU ON PREVOIT INCLUE
      
      # Données d'entrainement (historique jusqu'à i)
      df_train <- data.frame(
        var_y = y[train_idx], 
        var1  = p1_vec[train_idx], 
        var2  = p2_vec[train_idx]
      )
      
      # Données pour la prévision (i)
      df_prev <- data.frame(
        var_y = y[i], 
        var1  = p1_vec[i], 
        var2  = p2_vec[i]
      )
      
      # Régression
      fit <- lm(var_y ~ var1 + var2, data = df_train)
      
      if (!is.null(fit)) {
        # Prédiction
        pred_val <- predict(fit, newdata = df_prev)
        nowcast_vec[i] <- pred_val
        
        # Stockage poids
        weights_mat[row_id, ] <- coef(fit)
      }
      
      row_id <- row_id + 1
    }
    
    # Stockage résultats du mois j
    nowcast_df[, j] <- nowcast_vec
    
    # Nettoyage matrice poids
    weights_mat <- weights_mat[1:(row_id - 1), , drop = FALSE]
    weights_list[[paste0("Mois_", j)]] <- weights_mat
  }
  
  nowcast_df <- na.omit(nowcast_df)
  attr(nowcast_df, "na.action") <- NULL
  
  return(list(
    forecast_comb = nowcast_df,
    weights = weights_list
  ))
}







###################################################################################
# FORECAST COMB : MODELES TYPES ROLLING TRAITEMNT COVID
##########################################################################

rolling_inversed_weight_month_v2 <- function(y, model_list, dates, start_covid, end_covid, rolling_window) {
  
  #Vérif type et taille des inputs
  if (!is.numeric(y)) stop("y doit être un vecteur numérique.")
  n <- length(y)
  n_models <- length(model_list)
  
  for (k in 1:n_models) {
    if (nrow(model_list[[k]]) != n) stop(paste("Le modèle", k, "n'a pas le même nombre de lignes que y."))
  }
  
  # Conversion dates 
  dates <- as.Date(dates)
  start_covid <- as.Date(start_covid)
  end_covid <- as.Date(end_covid)
  
  # Date covid (true = date hors covid)
  is_safe_date <- !(dates >= start_covid & dates <= end_covid)
  
  # 3 colonnes (mois 1 -3=) par modèles
  nowcast_df <- data.frame(matrix(NA_real_, nrow = n, ncol = 3))
  colnames(nowcast_df) <- c("Mois_1", "Mois_2", "Mois_3")
  
  weights_list <- list()
  
  #Boucle sur chaque mois
  for (j in 1:3) {
    
    # matrice pour chaque modèle à un mois
    preds_mat <- do.call(cbind, lapply(model_list, function(df) df[, j]))
    colnames(preds_mat) <- paste0("Model_", 1:n_models)
    
    nowcast_vec <- rep(NA_real_, n)
    
    # Matrice pour stocker les poids 
    weights_storage <- matrix(NA_real_, nrow = n, ncol = n_models)
    colnames(weights_storage) <- colnames(preds_mat)
    
    #Rolling : on commence juste après la fenêtre
    for (i in (rolling_window + 1):n) {
      
      
      #on garde uniquement les dates non covid
      past_indices <- 1:(i - 1)
      valid_past_indices <- past_indices[is_safe_date[past_indices]]
      
      #Vérif si assez de dates
      if (length(valid_past_indices) >= rolling_window) {
        
        # Prendre les dates les plus récentes ( de la taille window)
        train_idx <- tail(valid_past_indices, rolling_window)
        
        # calcul des erreurs
        
        errors_mat <- preds_mat[train_idx, , drop = FALSE] - y[train_idx]
        
        # MSE pour chaque modèle 
        mses <- colMeans(errors_mat^2, na.rm = TRUE)
        
        # Inverse MSE
        w_raw <- 1 / mses
        
        # Si 0 ou inf
        if (any(is.infinite(w_raw))) w_raw[is.infinite(w_raw)] <- 1e10 # si infini
        if (all(is.na(w_raw))) w_raw <- rep(1, n_models) # si tout est NA
        
        # Normalisation
        w_norm <- w_raw / sum(w_raw, na.rm = TRUE)
        
        #Prévision
        preds_at_i <- preds_mat[i, ]
        if (!any(is.na(preds_at_i)) && !any(is.na(w_norm))) {
          nowcast_vec[i] <- sum(w_norm * preds_at_i)
          weights_storage[i, ] <- w_norm
        }
      }
    }
    
    nowcast_df[, j] <- nowcast_vec
    # Nettoyage des lignes vides
    weights_list[[paste0("Mois_", j)]] <- na.omit(weights_storage)
  }
  
  # Nettoyage final 
  final_nowcast <- na.omit(nowcast_df)
  attr(final_nowcast, "na.action") <- NULL
  
  return(list(
    nowcast = final_nowcast,
    weights = weights_list
  ))
}
  



gr_rolling_month_v2 <- function(y, model_list, dates, start_covid, end_covid, rolling_window) {
    
    # Vérif
    if (!is.numeric(y)) stop("y doit être numérique.")
    n <- length(y)
    n_models <- length(model_list)
    
    dates <- as.Date(dates)
    start_covid <- as.Date(start_covid)
    end_covid <- as.Date(end_covid)
    
    # Filtre Covid pour le training
    is_safe_date <- !(dates >= start_covid & dates <= end_covid)
    
    # Initialiser
    nowcast_df <- data.frame(matrix(NA_real_, nrow = n, ncol = 3))
    colnames(nowcast_df) <- c("Mois_1", "Mois_2", "Mois_3")
    weights_list <- list()
    
    #Boucle pour chaque mois
    for (j in 1:3) {
      
      # Matrice des prévisions brute pour le mois j
      preds_mat <- do.call(cbind, lapply(model_list, function(df) df[, j]))
      # Noms génériques pour la reg
      colnames(preds_mat) <- paste0("X", 1:n_models)
      
      nowcast_vec <- rep(NA_real_, n)
      
      # Stockage poids (Intercept + N modèles)
      weights_storage <- matrix(NA_real_, nrow = n, ncol = n_models + 1)
      colnames(weights_storage) <- c("(Intercept)", colnames(preds_mat))
      
      #Rolling (même logique que le modèle mse)
      for (i in (rolling_window + 1):n) {
        
        past_indices <- 1:(i - 1)
        valid_past_indices <- past_indices[is_safe_date[past_indices]]
        
        if (length(valid_past_indices) >= rolling_window) {
          train_idx <- tail(valid_past_indices, rolling_window)
          
          # DF Train : y et les modèles aux dates train_idx
          df_train <- data.frame(y = y[train_idx], preds_mat[train_idx, , drop = FALSE])
          
          # DF Prev : données des modèles à la date i 
          df_prev <- data.frame(preds_mat[i, , drop = FALSE])
          
          # Regression
          formula_str <- paste("y ~", paste(colnames(preds_mat), collapse = " + ")) #formule générale (appremment plus robuste)
          fit <- lm(as.formula(formula_str), data = df_train)
          pred_val <- predict(fit, newdata = df_prev)
          nowcast_vec[i] <- pred_val
              
            # Stockage des coefs
             weights_storage[i, ] <- coef(fit)
            
  
        }
      }
      
      nowcast_df[, j] <- nowcast_vec
      weights_list[[paste0("Mois_", j)]] <- na.omit(weights_storage)
    }
    
    final_nowcast <- na.omit(nowcast_df)
    attr(final_nowcast, "na.action") <- NULL
    
    return(list(
      forecast_comb = final_nowcast,
      weights = weights_list
    ))
  }








#Test sans suppression covid
####################################################################################################


gr_rolling_month_standard <- function(y, model_list, rolling_window) {
  

  if (!is.numeric(y)) stop("y doit être numérique.")
  n <- length(y)
  n_models <- length(model_list)
    nowcast_df <- data.frame(matrix(NA_real_, nrow = n, ncol = 3))
  colnames(nowcast_df) <- c("Mois_1", "Mois_2", "Mois_3")
  weights_list <- list()
  

  for (j in 1:3) {
    
    # Matrice Prévisions
    preds_mat <- do.call(cbind, lapply(model_list, function(df) df[, j]))
    colnames(preds_mat) <- paste0("X", 1:n_models)
    
    nowcast_vec <- rep(NA_real_, n)
    
    # Stockage poids
    weights_storage <- matrix(NA_real_, nrow = n, ncol = n_models + 1)
    colnames(weights_storage) <- c("(Intercept)", colnames(preds_mat))
    
    # Rolling
    for (i in (rolling_window + 1):n) {
      

      train_idx <- (i - rolling_window):(i - 1)
      
      # Préparation Df
      df_train <- data.frame(y = y[train_idx], preds_mat[train_idx, , drop = FALSE])
      df_prev  <- data.frame(preds_mat[i, , drop = FALSE])
      
      # Régression
      formula_str <- paste("y ~", paste(colnames(preds_mat), collapse = " + "))
    
        fit <- lm(as.formula(formula_str), data = df_train)
    
          # Prévision
          pred_val <- predict(fit, newdata = df_prev)
          nowcast_vec[i] <- pred_val
          
          # Stockage
          weights_storage[i, ] <- coef(fit)
    }
    
    nowcast_df[, j] <- nowcast_vec
    weights_list[[paste0("Mois_", j)]] <- na.omit(weights_storage)
  }
  
  final_nowcast <- na.omit(nowcast_df)
  attr(final_nowcast, "na.action") <- NULL
  
  return(list(
    forecast_comb = final_nowcast,
    weights = weights_list
  ))
}




rolling_inversed_weight_month_standard <- function(y, model_list, rolling_window) {
  
  n <- length(y)
  n_models <- length(model_list)
  
  for (k in 1:n_models) {
    if (nrow(model_list[[k]]) != n) stop(paste("Le modèle", k, "n'a pas le même nombre de lignes que y."))
  }
  
  nowcast_df <- data.frame(matrix(NA_real_, nrow = n, ncol = 3))
  colnames(nowcast_df) <- c("Mois_1", "Mois_2", "Mois_3")
  
  weights_list <- list()
  
  # Boucle mois
  for (j in 1:3) {
    
    # Matrice des prévisions pour le mois j
    preds_mat <- do.call(cbind, lapply(model_list, function(df) df[, j]))
    colnames(preds_mat) <- paste0("Model_", 1:n_models)
    
    nowcast_vec <- rep(NA_real_, n)
    weights_storage <- matrix(NA_real_, nrow = n, ncol = n_models)
    colnames(weights_storage) <- colnames(preds_mat)
    
    # Boucle Rolling
    for (i in (rolling_window + 1):n) {
      
      train_idx <- (i - rolling_window):(i - 1)
      
      
      # Erreurs sur la fenêtre passée
      errors_mat <- preds_mat[train_idx, , drop = FALSE] - y[train_idx]
      
      # MSE
      mses <- colMeans(errors_mat^2, na.rm = TRUE)
      
      # Inverse MSE
      w_raw <- 1 / mses
      
      # Normalisation
      w_norm <- w_raw / sum(w_raw, na.rm = TRUE)
      
      # Prévision
      preds_at_i <- preds_mat[i, ]
      
      if (!any(is.na(preds_at_i)) && !any(is.na(w_norm))) {
        nowcast_vec[i] <- sum(w_norm * preds_at_i)
        weights_storage[i, ] <- w_norm
      }
    }
    
    nowcast_df[, j] <- nowcast_vec
    weights_list[[paste0("Mois_", j)]] <- na.omit(weights_storage)
  }
  
  final_nowcast <- na.omit(nowcast_df)
  attr(final_nowcast, "na.action") <- NULL
  
  return(list(
    nowcast = final_nowcast,
    weights = weights_list
  ))
}