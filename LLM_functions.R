## FONCTION UTILITAIRES ##


#############
# Multi-usages
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

# Rechercher l'EMC du lendemain de la date de prévision
get_next_doc <- function(cur_day) {
  # target_date is Date of the corresponding EMC
  target_date <- cur_day + 1L
  next_doc <- date_publi_prev |>
    filter(date_finale_d == as.Date(target_date))
  return(next_doc$fichier)
}


# path_from_docname : renvoie chemin complet vers le ou les PDF local/locaux
path_from_docname <- function(doc_name, folder) {
  if (is.null(doc_name)) {
    return(NULL)
  }

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


# Dirigeant de l'INSEE selon la date
INSEE_current_boss <- function(y_p) {
  if (y_p <= as.Date("2012-02-21")) {
    return("Jean-Philippe Cotis")
  } else if (y_p <= as.Date("2025-05-31")) {
    return("Jean-Luc Tavernier")
  } else {
    return("Fabrice Lenglart")
  }
}


# Dirigeant de la BDF selon la date
BDF_current_boss <- function(y_p) {
  if (y_p <= as.Date("2002-12-31")) {
    return("Jean-Claude Trichet")
  } else if (y_p <= as.Date("2015-10-31")) {
    return("Christian Noyer")
  } else {
    return("François Villeroy de Galhau")
  }
}


# Concaténer les enquêtes de l'INSEE/BDF en un PDF
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

  # Chercher l'enquête du mois précédent (rollback ramène au dernier jour du mois d'avant)
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
    year = as.integer(str_extract(all_files, "^\\d{4}")),
    month = as.integer(str_extract(all_files, "(?<=\\d{4}_)\\d{2}"))
  ) |>
    mutate(doc_date = ymd(paste(year, month, "01", sep = "-")))

  # On cherche l'enquête du mois d'avant qui, par transformation, sera toujours au 1er du mois
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



#######################
# LLM_all_inputs
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
# LLM_excel_with_error
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
  if ("dates" %in% names(df_PIB)) {
    vec <- as.Date(df_PIB$dates)
  } else if ("Date" %in% names(df_PIB)) {
    vec <- as.Date(df_PIB$Date)
  } else {
    vec <- as.Date(df_PIB[[1]])
  }
  # match date exact
  exact <- which(vec == cur_date)
  if (length(exact) == 1) {
    return(exact)
  }
  # match par trimestre
  q_target <- quarter(cur_date)
  y_target <- year(cur_date)
  qidx <- which(quarter(vec) == q_target & lubridate::year(vec) == y_target)
  if (length(qidx) >= 1) {
    return(qidx[1])
  }
  # match avec le dernier PIB en date si pas dans le trimestre
  prev_idx <- which(vec <= cur_date)
  if (length(prev_idx) >= 1) {
    return(tail(prev_idx, 1))
  }
  # si aucune correspondance alors renvoi le prochain PIB (laisser ça ou faire une erreur ?)
  future_idx <- which(vec > cur_date)
  if (length(future_idx) >= 1) {
    return(future_idx[1])
  }
  integer(0)
}


#####################
# STATS DES
#####################
# Passage en long pour stat des plus simple à rédiger

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


########################################
# COMP RESULTS et COMP RESULTS FUNCTION
########################################

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
      .names = "error_{.col}"
    ))
}

# RMSE des modèles sur la période
calculate_eco_rmse <- function(df_eco_filtered, specific_ind = FALSE) {
  calc_rmse <- function(df, model_name, prefix) {
    df |>
      na.omit() |>
      summarise(
        Model = model_name,
        RMSE_M1 = sqrt(mean((PIB_PR - get(paste0(prefix, "_M1")))^2)),
        RMSE_M2 = sqrt(mean((PIB_PR - get(paste0(prefix, "_M2")))^2)),
        RMSE_M3 = sqrt(mean((PIB_PR - get(paste0(prefix, "_M3")))^2))
      )
  }
  
  df_bdf <- df_eco_filtered |> select(PIB_PR, starts_with("BDF_ALL"))
  df_insee <- df_eco_filtered |> select(PIB_PR, starts_with("INSEE_ALL"))
  df_comb <- df_eco_filtered |> select(PIB_PR, starts_with("COMB_ALL"))
  
  res <- bind_rows(
    calc_rmse(df_bdf, "ECO_BDF", "BDF_ALL"),
    calc_rmse(df_insee, "ECO_INSEE", "INSEE_ALL"),
    calc_rmse(df_comb, "ECO_ALL", "COMB_ALL")
  )
  
  if (specific_ind) {
    df_ar <- df_eco_filtered |> select(PIB_PR, starts_with("AR"))
    df_bdf_ind <- df_eco_filtered |> select(PIB_PR, starts_with("BDF_IND"))
    df_insee_ind <- df_eco_filtered |> select(PIB_PR, starts_with("INSEE_IND"))
    
    res_ind <- bind_rows(
      calc_rmse(df_ar, "ECO_AR", "AR"),
      calc_rmse(df_bdf_ind, "ECO_BDF_IND", "BDF_IND"),
      calc_rmse(df_insee_ind, "ECO_INSEE_IND", "INSEE_IND")
    )
    res <- bind_rows(res, res_ind)
  }
  return(res)
}

# Prépare rank des RMSE
plot_ranking_rmse <- function(metrics_data, title_plot, subtitle_plot, target_models = NULL) {
  df_plot <- metrics_data
  
  # Couleurs pour les graphiques
  CUSTOM_COLORS <- c(
    "LLM BDF" = "cornflowerblue",
    "LLM INSEE" = "cyan4",
    "LLM ALL" = "cyan",
    "ECONOMETRICS" = "coral1"
  )
  
  if (!is.null(target_models)) {
    df_plot <- df_plot |> filter(Model %in% target_models)
  }
  
  df_plot <- df_plot |>
    pivot_longer(cols = starts_with("RMSE"), names_to = "Mois", values_to = "RMSE") |>
    mutate(
      Model = ifelse(Model == "all_txt", "ALL_txt", Model),
      Type = case_when(
        str_detect(Model, "^BDF") ~ "LLM BDF",
        str_detect(Model, "^INSEE") ~ "LLM INSEE",
        str_detect(Model, "^ECO") ~ "ECONOMETRICS",
        TRUE ~ "LLM ALL"
      ),
      Mois = str_replace(Mois, "RMSE_", "")
    ) |>
    group_by(Mois) |>
    mutate(Model_Ordered = reorder_within(Model, -RMSE, Mois)) |>
    ungroup()
  
  p <- ggplot(df_plot, aes(x = RMSE, y = Model_Ordered, fill = Type)) +
    geom_col(width = 0.7, alpha = 0.9) +
    geom_text(aes(label = round(RMSE, 3)), hjust = -0.2, size = 3.5, fontface = "bold") +
    facet_wrap(~Mois, scales = "free_y", ncol = 3) +
    scale_y_reordered() +
    scale_fill_manual(values = CUSTOM_COLORS) +
    labs(title = title_plot, subtitle = subtitle_plot, x = "RMSE", y = "", fill = "Model") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.background = element_rect(fill = "#2C3E50", color = NA),
      strip.text = element_text(color = "white", face = "bold", size = 10),
      panel.grid.major.y = element_blank(),
      legend.position = "top"
    ) +
    expand_limits(x = max(df_plot$RMSE) * 1.15)
  
  print(p)
}

#Median des variables d'intérêt
prep_stats <- function(path, src) {
  read_xlsx(path) |>
    rowwise() |>
    mutate(
      median_forecast = median(c_across(starts_with("forecast_")), na.rm = T),
      median_conf = median(c_across(starts_with("confidence_")), na.rm = T), source = src
    ) |>
    select(Date, median_forecast, median_conf, source) |>
    ungroup()
}


# Même type de fonction pour le graphique que plot_ranking_rmse
plot_ranking_rmse_sent <- function(metrics_data, title_plot, subtitle_plot) {
  
  # Code couleur
  CUSTOM_COLORS <- c(
    "LLM" = "cornflowerblue",
    "Mixed" = "#27AE60",
    "ECONOMETRICS" = "coral1"
  )
  
  

  df_plot <- metrics_data |>
    pivot_longer(cols = starts_with("RMSE"), names_to = "Mois", values_to = "RMSE") |>
    mutate(
      Category = case_when(
        str_detect(Model, "(?i)txt|text") ~ "LLM",
        str_detect(Model, "(?i)ECO|Climate|Climat") ~ "ECONOMETRICS",
        TRUE ~ "Mixed"
      ),
      
      # Nettoyer les noms
      Model_Clean = Model,
      Model_Clean = str_remove_all(Model_Clean, "(?i)climate|climat"),
      Model_Clean = str_replace_all(Model_Clean, "(?i)sentiment", "Sent"),
      Model_Clean = str_replace_all(Model_Clean, "(?i)ind", "IND"),
      Model_Clean = str_replace_all(Model_Clean, "(?i)all", "ALL"),
      
      
      # Label Mois
      Mois = str_replace(Mois, "RMSE_", "")
    ) |>
    group_by(Mois) |>
    mutate(Model_Ordered = reorder_within(Model_Clean, -RMSE, Mois)) |>
    ungroup()
  
  # graphique
  ggplot(df_plot, aes(x = RMSE, y = Model_Ordered, fill = Category)) +
    geom_col(width = 0.75, alpha = 0.85) +
    
    # Paramètres valeurs affichées
    geom_text(aes(label = round(RMSE, 3)),
              hjust = -0.15, size = 3.2, fontface = "bold", color = "grey20"
    ) +
    
    # Facet
    facet_wrap(~Mois, scales = "free_y", ncol = 3) +
    scale_y_reordered() +
    scale_fill_manual(values = CUSTOM_COLORS) +
    labs(
      title = title_plot,
      subtitle = subtitle_plot,
      x = "RMSE",
      y = "",
      fill = "Model Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.background = element_rect(fill = "#34495E", color = NA),
      strip.text = element_text(color = "white", face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box.margin = margin(t = 10)
    ) +
    expand_limits(x = max(df_plot$RMSE) * 1.2)
}




##################################################################################
#Cutoff analysis
###############################################################################


###########################
# Fonction analyse cutoff
###########################
analyze_cutoff_accuracy <- function(file_path, model_name, pib_data, cutoff_date, dummy) {
  df_model <- read_xlsx(file_path)
  
  if (dummy == 0) {
    df_model <- df_model |>
      filter(!(Date > "2020-01-01" & Date <= "2021-01-12"))
  }
  
  # B. Nettoyage
  df_prep <- df_model |>
    mutate(Date = as.Date(Date)) |>
    rowwise() |>
    mutate(median_f = median(c_across(starts_with("forecast_")), na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      month = month(Date),
      year = year(Date),
      forecast_quarter = case_when(
        month %in% c(2, 3, 4) ~ 1, month %in% c(5, 6, 7) ~ 2,
        month %in% c(8, 9, 10) ~ 3, month %in% c(11, 12, 1) ~ 4
      ),
      forecast_year = case_when(month == 1 ~ year - 1, TRUE ~ year)
    )
  
  # Join avec le PIB
  df_joined <- left_join(df_prep, pib_data, by = c("forecast_year", "forecast_quarter"))
  
  # Erreur absolue
  df_errors <- df_joined |>
    filter(!is.na(PIB_PR)) |>
    mutate(
      Error = PIB_PR - median_f,
      Abs_Error = abs(Error),
      Period = ifelse(Date.x < cutoff_date, "Pre-Cutoff", "Post-Cutoff")
    )
  
  
  # Stats
  n_pre <- sum(df_errors$Period == "Pre-Cutoff", na.rm = TRUE)
  n_post <- sum(df_errors$Period == "Post-Cutoff", na.rm = TRUE)
  
  
  # Initialisation des variables
  p_val <- NA
  
  
  # Test (si assez de données)
  if (n_pre > 1 & n_post > 1) {
    try(
      {
        test_res <- t.test(Abs_Error ~ Period, data = df_errors, var.equal = FALSE)
        p_val <- test_res$p.value
      },
      silent = TRUE
    )
  }
  
  # Stats
  summary_stats <- df_errors |>
    group_by(Period) |>
    summarise(
      MAE = mean(Abs_Error, na.rm = TRUE),
      RMSE = sqrt(mean(Error^2, na.rm = TRUE)),
      N_Obs = n(),
      .groups = "drop"
    ) |>
    pivot_wider(names_from = Period, values_from = c(MAE, RMSE, N_Obs))
  
  # Si période manquante
  if (!"MAE_Post-Cutoff" %in% names(summary_stats)) summary_stats$`MAE_Post-Cutoff` <- NA
  if (!"MAE_Pre-Cutoff" %in% names(summary_stats)) summary_stats$`MAE_Pre-Cutoff` <- NA
  
  summary_stats <- summary_stats |>
    mutate(
      Model = model_name,
      P_Value_T_Test = round(p_val, 4),
      Diff_MAE = `MAE_Post-Cutoff` - `MAE_Pre-Cutoff`,
      Significatif = ifelse(!is.na(P_Value_T_Test) & P_Value_T_Test < 0.05, "OUI", "NON")
    ) |>
    select(Model, any_of(c("MAE_Pre-Cutoff", "MAE_Post-Cutoff", "Diff_MAE", "Significatif", "P_Value_T_Test")), everything())
  
  return(summary_stats)
}


###########################
# Fonction analyse période
###########################
analyze_period_accuracy <- function(file_path, model_name, pib_data, cutoff_date, dummy) {
  df_model <- read_xlsx(file_path)
  
  if (dummy == 0) {
    df_model <- df_model |>
      filter(!(Date >= "2020-02-01" & Date <= "2021-02-01"))
  }
  
  # B. Nettoyage
  df_prep <- df_model |>
    mutate(Date = as.Date(Date)) |>
    rowwise() |>
    mutate(median_f = median(c_across(starts_with("forecast_")), na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      month = month(Date),
      year = year(Date),
      forecast_quarter = case_when(
        month %in% c(2, 3, 4) ~ 1, month %in% c(5, 6, 7) ~ 2,
        month %in% c(8, 9, 10) ~ 3, month %in% c(11, 12, 1) ~ 4
      ),
      forecast_year = case_when(month == 1 ~ year - 1, TRUE ~ year)
    )
  
  # Join avec le PIB
  df_joined <- left_join(df_prep, pib_data, by = c("forecast_year", "forecast_quarter"))
  
  # Erreur absolue
  df_errors <- df_joined |>
    filter(!is.na(PIB_PR)) |>
    mutate(
      Errors = PIB_PR - median_f,
      Abs_Error = abs(Errors),
      Period = ifelse(Date.x < cutoff_1, "Period1", "Period2")
    )
  
  
  # Stats
  n_pre <- sum(df_errors$Period == "Period1", na.rm = TRUE)
  n_post <- sum(df_errors$Period == "Period2", na.rm = TRUE)
  
  
  # Initialisation des variables
  p_val <- NA
  
  
  # Test (si assez de données)
  if (n_pre > 1 & n_post > 1) {
    try(
      {
        test_res <- t.test(Abs_Error ~ Period, data = df_errors, var.equal = FALSE)
        p_val <- test_res$p.value
      },
      silent = TRUE
    )
  }
  
  # Stats
  summary_stats <- df_errors |>
    group_by(Period) |>
    summarise(
      MAE = mean(Abs_Error, na.rm = TRUE),
      RMSE = sqrt(mean(Errors^2, na.rm = TRUE)),
      N_Obs = n(),
      .groups = "drop"
    ) |>
    pivot_wider(names_from = Period, values_from = c(MAE, RMSE, N_Obs))
  
  # Si période manquante
  if (!"MAE_Period1" %in% names(summary_stats)) summary_stats$`MAE_Period1` <- NA
  if (!"MAE_Period2" %in% names(summary_stats)) summary_stats$`MAE_Period2` <- NA
  
  summary_stats <- summary_stats |>
    mutate(
      Model = model_name,
      P_Value_T_Test = round(p_val, 4),
      Diff_MAE = `MAE_Period2` - `MAE_Period1`,
      Significatif = ifelse(!is.na(P_Value_T_Test) & P_Value_T_Test < 0.05, "OUI", "NON")
    ) |>
    select(Model, any_of(c("MAE_Period1", "MAE_Period2", "Diff_MAE", "Significatif", "P_Value_T_Test")), everything())
  
  return(summary_stats)
}



#############################
# Period analysis monthly
############################

analyze_period_monthly_accuracy <- function(file_path, model_name, pib_data, dummy_covid) {
  df_model <- read_xlsx(file_path)
  
  # Filtre COVID
  if (dummy_covid == 0) {
    df_model <- df_model |>
      filter(!(Date >= as.Date("2020-02-01") & Date < as.Date("2022-02-01")))
  }
  
  # Nettoyage
  df_prep <- df_model |>
    mutate(Date = as.Date(Date)) |>
    rowwise() |>
    mutate(median_f = median(c_across(starts_with("forecast_")), na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      month = month(Date),
      year = year(Date),
      forecast_quarter = case_when(
        month %in% c(2, 3, 4) ~ 1, month %in% c(5, 6, 7) ~ 2,
        month %in% c(8, 9, 10) ~ 3, month %in% c(11, 12, 1) ~ 4
      ),
      # Tri par mois dans le trimestre
      month_in_quarter = case_when(
        month %in% c(2, 5, 8, 11) ~ 1, month %in% c(3, 6, 9, 12) ~ 2,
        month %in% c(4, 7, 10, 1) ~ 3
      ),
      forecast_year = case_when(month == 1 ~ year - 1, TRUE ~ year)
    )
  
  # Join
  df_joined <- left_join(df_prep, pib_data, by = c("forecast_year", "forecast_quarter"))
  
  # Calcul Err abs sur la période
  df_errors <- df_joined |>
    filter(!is.na(PIB_PR)) |>
    mutate(
      Abs_Error = abs(PIB_PR - median_f),
      Error_Squared = (PIB_PR - median_f)^2,
      Period_Group = case_when(
        forecast_year < 2020 ~ "2015_2019",
        forecast_year >= 2020 ~ "2020_2025"
      )
    )
  
  # Agrégation par mois et par période
  summary_stats <- df_errors |>
    group_by(Period_Group, month_in_quarter) |>
    summarise(
      MAE = mean(Abs_Error, na.rm = TRUE),
      RMSE = sqrt(mean(Error_Squared, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # F. PIVOTAGE POUR CRÉER UNE LIGNE PAR MODÈLE
  final_wide <- summary_stats |>
    # Pour avoir une colonne par mois dans le trimestre
    pivot_wider(
      names_from = c(Period_Group, month_in_quarter),
      values_from = c(MAE, RMSE),
      names_sep = "_Mois_",
      names_glue = "{.value}_{Period_Group}_M{month_in_quarter}"
    ) |>
    # Changement de nom de variable
    mutate(
      Model = model_name
    ) |>
    # SÉLECTION ET TRI DES COLONNES DE SORTIE
    select(Model, starts_with("MAE"), starts_with("RMSE"))
  
  return(final_wide)
}






#################################################################################
# FONCTION LLM + ECONOMETRIE
#################################################################################


## Fonction utilitaire pour calculer les erreurs
calc_errors <- function(obs, preds) {
  err <- preds - obs
  c(MAE = mean(abs(err)), RMSE = sqrt(mean(err^2)))
}
metrics_list <- list()


#############################################################
# Forecast combination (moyennes simples)
###############################################


#  Fonction moyenne arithmétique

simple_avg_month <- function(models) {
  n_models <- length(models)
  n_rows <- nrow(models[[1]])

  # Modèles doivent avoir la même taille
  for (k in 1:n_models) {
    if (nrow(models[[k]]) != n_rows) stop(paste("Le modèle", k, "n'a pas le même nombre de lignes."))
  }

  # df de sortie
  avg_forecasts <- data.frame(matrix(NA_real_, nrow = n_rows, ncol = 3))
  colnames(avg_forecasts) <- c("Mois_1", "Mois_2", "Mois_3")


  for (j in 1:3) {
    #
    preds_mat <- do.call(cbind, lapply(models, function(df) df[, j]))

    # Moyenne ligne par ligne
    avg_forecasts[, j] <- rowMeans(preds_mat, na.rm = TRUE)
  }

  # Nettoyage pour ne pas voir des NA en bas du tableau qd on affiche
  final_nowcast <- na.omit(avg_forecasts)
  attr(final_nowcast, "na.action") <- NULL

  return(final_nowcast)
}


rolling_confidence_weight <- function(model_list, list_confidence) {
  n_rows <- nrow(model_list[[1]])
  n_cols <- 3 # Colonne pour chaque mois

  # Matrice pour stocker la prev
  final_comb_forecast <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(final_comb_forecast) <- c("Mois_1", "Mois_2", "Mois_3")

  # Boucle sur les 3 mois
  for (j in 1:n_cols) {
    # Prev pour un mois j
    preds_mat <- sapply(names(model_list), function(name) {
      if (is.data.frame(model_list[[name]])) {
        return(model_list[[name]][, j])
      } else {
        return(model_list[[name]])
      }
    })

    # Confiance pour un mois
    conf_mat <- sapply(names(list_confidence), function(name) {
      return(list_confidence[[name]][, j])
    })


    # Normaliser les poids
    sum_conf_rows <- rowSums(conf_mat, na.rm = TRUE)

    # Calcul des poids normalisés
    weights_mat <- conf_mat / sum_conf_rows

    # Calcul de la prev combinée
    weighted_preds <- rowSums(weights_mat * preds_mat, na.rm = TRUE)

    final_comb_forecast[, j] <- weighted_preds
  }

  return(final_comb_forecast)
}
###################################################################################
# FORECAST COMB : MODELES TYPES ROLLING TRAITEMENT COVID
##########################################################################


# Combinaison : poids normalisés obtenus via l'inverse de leur MSE
## Fenêtre d'entraînement pour obtenir ces poids
rolling_inversed_weight_month <- function(y, model_list, dates, start_covid, end_covid, rolling_window) {
  # Vérif type et taille des inputs
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

  # Boucle sur chaque mois
  for (j in 1:3) {
    # matrice pour chaque modèle à un mois
    preds_mat <- do.call(cbind, lapply(model_list, function(df) df[, j]))
    colnames(preds_mat) <- paste0("Model_", 1:n_models)

    nowcast_vec <- rep(NA_real_, n)

    # Matrice pour stocker les poids
    weights_storage <- matrix(NA_real_, nrow = n, ncol = n_models)
    colnames(weights_storage) <- colnames(preds_mat)

    # Rolling : on commence juste après la fenêtre
    for (i in (rolling_window + 1):n) {
      # on garde uniquement les dates non covid
      past_indices <- 1:(i - 1)
      valid_past_indices <- past_indices[is_safe_date[past_indices]]

      # Vérif si assez de dates
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

        # Prévision
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


# Combinaison à la Granger-Ramanathan : poids non normalisé, regressions sans contraintes.
## On obtient les poids en régressant le PIB observé sur ses forecasts dans une fenêtre d'entraînement
gr_rolling_month <- function(y, model_list, dates, start_covid, end_covid, rolling_window) {
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

  # Boucle pour chaque mois
  for (j in 1:3) {
    # Matrice des prévisions brute pour le mois j
    preds_mat <- do.call(cbind, lapply(model_list, function(df) df[, j]))
    # Noms génériques pour la reg
    colnames(preds_mat) <- paste0("X", 1:n_models)

    nowcast_vec <- rep(NA_real_, n)

    # Stockage poids (Intercept + N modèles)
    weights_storage <- matrix(NA_real_, nrow = n, ncol = n_models + 1)
    colnames(weights_storage) <- c("(Intercept)", colnames(preds_mat))

    # Rolling (même logique que le modèle mse)
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
        formula_str <- paste("y ~", paste(colnames(preds_mat), collapse = " + ")) # formule générale (appremment plus robuste)
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


# Test sans suppression covid
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
      df_prev <- data.frame(preds_mat[i, , drop = FALSE])

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
