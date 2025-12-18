source("Library_Nowcasting_LLM.R")

# Conversion en format long
prepare_long_data <- function(df, version_name) {
  df |>
    mutate(Date = as.Date(Date)) |>
    rename(Date = Date) |>
    pivot_longer(
      cols = starts_with("forecast_"),
      names_to = "Forecast_ID",
      values_to = "Value"
    ) |>
    mutate(Version = version_name)
}

# Comparaison des Différences
compare_forecast_differences <- function(file_path_initial, file_path_2020, model_name) {
  ## Lecture et préparation des données en format Long
  df_initial <- read_xlsx(file_path_initial) |> prepare_long_data("Initial")
  df_2020 <- read_xlsx(file_path_2020) |> prepare_long_data("V2020")

  # Liste des dates de la Version 2020
  dates_v2020 <- unique(df_2020$Date)

  # join
  comparison_long <- full_join(
    df_2020, df_initial,
    by = c("Date", "Forecast_ID"),
    suffix = c("_V2020", "_Initial")
  ) |>
    # Filtrer uniquement les dates présentes dans la version 2020
    filter(Date %in% dates_v2020) |>
    # Calculer la différence pour chaque cellule
    mutate(
      is_different = case_when(
        # Cas 1 : La valeur existe dans une version et pas l'autre
        xor(is.na(Value_V2020), is.na(Value_Initial)) ~ TRUE,

        # Cas 2 : Les deux sont non-NA et différents
        Value_V2020 != Value_Initial ~ TRUE,

        # Cas 3 : Les deux sont NA
        is.na(Value_V2020) & is.na(Value_Initial) ~ TRUE,

        # Cas 4 : Les deux sont non-NA et égaux (aucune différence)
        TRUE ~ FALSE
      )
    ) |>
    # Compter les différences par date
    group_by(Date) |>
    summarise(
      Model = model_name,
      Total_Forecasts_Compared = n(),
      Count_Differences = sum(is_different, na.rm = TRUE),
      .groups = "drop"
    ) |>
    select(Model, Date, Total_Forecasts_Compared, Count_Differences)

  return(comparison_long)
}


# APPEL DE LA FONCTION AVEC NOS SCRIPTS

file_pairs <- list(
  # Modèles BDF
  "BDF_text" = list(
    initial = "Results/BDF_text.xlsx",
    v2020 = "Final_results/BDF_text_2020.xlsx"
  ),
  "BDF_noText" = list(
    initial = "Results/BDF_noText.xlsx",
    v2020 = "Final_results/BDF_noText_2020.xlsx"
  ),
  "BDF_rolling" = list(
    initial = "Results/BDF_rolling_text.xlsx",
    v2020 = "Final_results/BDF_rolling_text_2020.xlsx"
  ),
  "BDF_all" = list(
    initial = "Results/BDF_all.xlsx",
    v2020 = "Final_results/BDF_all_2020.xlsx"
  ),
  "BDF_just_Text" = list(
    initial = "Results/BDF_just_text.xlsx",
    v2020 = "Final_results/BDF_just_text_2020.xlsx"
  ),
  "BDF_excel" = list(
    initial = "Results/BDF_excel.xlsx",
    v2020 = "Final_results/BDF_excel_2020.xlsx"
  ),
  "BDF_excel_error" = list(
    initial = "Results/BDF_excel_error.xlsx",
    v2020 = "Final_results/BDF_excel_error_2020.xlsx"
  ),
  "BDF_ECO_text" = list(
    initial = "Results/BDF_ECO_text.xlsx",
    v2020 = "Final_results/BDF_ECO_text_2020.xlsx"
  ),
  "BDF_text_FR" = list(
    initial = "Results/BDF_text_FR.xlsx",
    v2020 = "Final_results/BDF_text_FR_2020.xlsx"
  ),


  # Modèles INSEE
  "INSEE_text" = list(
    initial = "Results/INSEE_text.xlsx",
    v2020 = "Final_results/INSEE_text_2020.xlsx"
  ),
  "INSEE_noText" = list(
    initial = "Results/INSEE_noText.xlsx",
    v2020 = "Final_results/INSEE_noText_2020.xlsx"
  ),
  "INSEE_rolling" = list(
    initial = "Results/INSEE_rolling_text.xlsx",
    v2020 = "Final_results/INSEE_rolling_text_2020.xlsx"
  ),
  "INSEE_all" = list(
    initial = "Results/INSEE_all.xlsx",
    v2020 = "Final_results/INSEE_all_2020.xlsx"
  ),
  "INSEE_just_Text" = list(
    initial = "Results/INSEE_just_text.xlsx",
    v2020 = "Final_results/INSEE_just_Text_2020.xlsx"
  ),
  "INSEE_excel" = list(
    initial = "Results/INSEE_excel.xlsx",
    v2020 = "Final_results/INSEE_excel_2020.xlsx"
  ),
  "INSEE_excel_error" = list(
    initial = "Results/INSEE_excel_error.xlsx",
    v2020 = "Final_results/INSEE_excel_error_2020.xlsx"
  ),
  "INSEE_ECO_text" = list(
    initial = "Results/INSEE_ECO_text.xlsx",
    v2020 = "Final_results/INSEE_ECO_text_2020.xlsx"
  ),
  "INSEE_text_FR" = list(
    initial = "Results/INSEE_text_FR.xlsx",
    v2020 = "Final_results/INSEE_text_FR_2020.xlsx"
  ),
  # Modèle ECO
  "ECO_text" = list(
    initial = "Results/ECO_text.xlsx",
    v2020 = "Final_results/ECO_text_2020.xlsx"
  )
)

# Liste pour stocker tous les résultats
all_difference_metrics <- list()

# Exécution de la boucle de comparaison
for (model_name in names(file_pairs)) {
  pair <- file_pairs[[model_name]]

  # Appel de la fonction pour chaque paire
  metrics <- compare_forecast_differences(
    file_path_initial = pair$initial,
    file_path_2020 = pair$v2020,
    model_name = model_name
  )


  all_difference_metrics[[model_name]] <- metrics
}

# Combinaison de tous les résultats en un seul DF
final_difference_table <- bind_rows(all_difference_metrics)

final_difference_table <- final_difference_table |>
  filter(Count_Differences > 0)
