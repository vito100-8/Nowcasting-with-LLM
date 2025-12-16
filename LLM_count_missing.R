## Liste des fichiers à analyser (extraite de votre script)
file_list_2020 <- list(
  BDF_txt    ="Final_results/BDF_text_2020.xlsx", 
  BDF_txtrol ="Final_results/BDF_rolling_text_2020.xlsx", 
  BDF_txtO   ="Final_results/BDF_just_text_2020.xlsx",
  BDF_txtTS  ="Final_results/BDF_all_2020.xlsx", 
  BDF_TS     ="Final_results/BDF_excel_2020.xlsx",
  BDF_txtFR  ="Final_results/BDF_text_FR_2020.xlsx",
  
  # --- Modèles INSEE ---
  INSEE_txt    ="Final_results/INSEE_text_2020.xlsx", 
  INSEE_txtrol ="Final_results/INSEE_rolling_text_2020.xlsx", 
  INSEE_txtO   ="Final_results/INSEE_just_text_2020.xlsx",
  INSEE_txtTS  ="Final_results/INSEE_all_2020.xlsx",
  INSEE_TS     ="Final_results/INSEE_excel_2020.xlsx",
  INSEE_txtFR  ="Final_results/INSEE_text_FR_2020.xlsx",
  
  # --- Modèles ECO ---
  ALL_txt ="Final_results/ECO_text_2020.xlsx")
  


# Fonction pour lire un fichier et compter les NA dans les colonnes de prévision
count_na_in_forecasts <- function(file_path, model_name) {
  
  if (!file.exists(file_path)) {
    warning(paste("Fichier non trouvé:", file_path))
    return(data.frame(Model = model_name, Total_Missing = NA_real_))
  }
  
  df <- read_xlsx(file_path)
  
  # Sélection des colonnes 'forecast_' et compte total des NA
  total_na <- df |>
    select(starts_with("forecast_")) |>
    is.na() |>
    sum()
  
  return(data.frame(Model = model_name, Total_Missing = total_na))
}


# Boucle pour calculer les NA pour chaque fichier
all_missing_counts <- list()

for (model_name in names(file_list_2020)) {
  file_path <- file_list_2020[[model_name]]
  result <- count_na_in_forecasts(file_path, model_name)
  
  all_missing_counts[[model_name]] <- result
}

#  tableau final
df_missing_summary <- bind_rows(all_missing_counts) 


# Préparation du tableau pour exportation
latex_table <- df_missing_summary |>
  rename(
    Model = Model,
    `Number of missing values` = Total_Missing
  ) |>
  #  code LaTeX
  kable(
    format = "latex",
    caption = "Non-responses across models",
    label = "tab:missing_values_summary",
    booktabs = TRUE,
    align = c('l', 'c')
  ) |>
  kable_styling(
    latex_options = "basic",
    full_width = FALSE
  )



  # Code laTex : 

cat(latex_table)
