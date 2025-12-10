# Script pour calculer le MAE et RMSE des modèles

source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")

################################################
# PRÉPARATION DES DONNÉES DE PIB
################################################

df_PIB <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")

# Nettoyage du PIB (La cible reste complète pour la jointure)
pib <- df_PIB |>
  mutate(Date = as.Date(dates), 
         forecast_year = year(Date), 
         Month = month(Date), 
         forecast_quarter = case_when( 
           Month == 2 ~ 1,
           Month == 5 ~ 2,
           Month == 8 ~ 3,
           Month == 11 ~ 4
         )
  )

################################################
# PARAMÈTRES GÉNÉRAUX
################################################

# DUMMY COVID
# 0 : Supprimer la période Covid 
# 1 : Conserver la période Covid
covid <- 1

# Fonction pour enlever obs covid
filter_covid_dates <- function(df, dummy) {
  if (dummy == 0) {
    df |> 
      filter(!(Date > "2020-01-01" & Date <= "2021-01-12"))
  } else {
    df
  }
}

# Fonction pour traiter chaque modèle
process_model <- function(file_path, model_name, pib_data, covid_dummy) {
  
  df_model <- read_xlsx(file_path) 
  
  df_long <- df_model |>
    filter_covid_dates(covid_dummy) |> 
    rowwise() |>
    mutate(Date = Date,
           median_forecast = median(c_across(starts_with("forecast_")), na.rm = TRUE),
           .keep = "none"
    ) |>
    ungroup() |>
    mutate(
      month = month(Date), year = year(Date)
    ) |>
    mutate(
      forecast_quarter = case_when(
        month %in% c(2, 3, 4) ~ 1, month %in% c(5, 6, 7) ~ 2,
        month %in% c(8, 9, 10) ~ 3, month %in% c(11, 12, 1) ~ 4
      ),
      month_in_quarter = case_when(
        month %in% c(2, 5, 8, 11) ~ 1, month %in% c(3, 6, 9, 12) ~ 2,
        month %in% c(4, 7, 10, 1) ~ 3
      ),
      forecast_year = case_when(month == 1 ~ year - 1, TRUE ~ year)
    )
  
  df_wide <- df_long |>
    pivot_wider(
      id_cols = c(forecast_year, forecast_quarter), 
      names_from = month_in_quarter, values_from = median_forecast,
      names_prefix = "Forecast_Mois_"
    ) |>
    arrange(forecast_year, forecast_quarter)
  
  forecast_merged <- left_join(df_wide, pib_data, join_by(forecast_year, forecast_quarter))
  
  metrics <- forecast_merged |>
    filter(!is.na(PIB_PR)) |>
    summarise(Model = model_name,
              MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
              MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
              MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
              RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
              RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
              RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))
  
  return(metrics)
}

################################################
# CALCUL MAE/RMSE PAR MODÈLE 
################################################

# --- Modèles BDF ---

metrics_BDF_txt <- process_model("Final_results/BDF_text_2020.xlsx", "BDF_txt", pib, covid)
metrics_BDF_txtrol <- process_model("Final_results/BDF_rolling_text_2020.xlsx", "BDF_txtrol", pib, covid)
metrics_BDF_txtO <- process_model("Final_results/BDF_just_text_2020.xlsx", "BDF_txtO", pib, covid)
metrics_BDF_txtTS <- process_model("Final_results/BDF_all_2020.xlsx", "BDF_txtTS", pib, covid)
metrics_BDF_TS <- process_model("Final_results/BDF_excel_2020.xlsx", "BDF_TS", pib, covid)
metrics_BDF_txtFR <- process_model("Final_results/BDF_text_FR_2020.xlsx", "BDF_txtFR", pib, covid)


# --- Modèles INSEE ---

metrics_INSEE_txt <- process_model("Final_results/INSEE_text_2020.xlsx", "INSEE_txt", pib, covid)
metrics_INSEE_notxt <- process_model("Final_results/INSEE_noText_2020.xlsx", "INSEE_notxt", pib, covid)
metrics_INSEE_txtrol <- process_model("Final_results/INSEE_rolling_text_2020.xlsx", "INSEE_txtrol", pib, covid)
metrics_INSEE_txtO <- process_model("Final_results/INSEE_just_text_2020.xlsx", "INSEE_txtO", pib, covid)
metrics_INSEE_txtTS <- process_model("Final_results/INSEE_all_2020.xlsx", "INSEE_txtTS", pib, covid)
metrics_INSEE_TS <- process_model("Final_results/INSEE_excel_2020.xlsx", "INSEE_TS", pib, covid)
metrics_INSEE_txtFR <- process_model("Final_results/INSEE_text_FR_2020.xlsx", "INSEE_txtFR", pib, covid)


# --- Modèles ECO ---

metrics_all_txt <- process_model("Final_results/ECO_text_2020.xlsx", "ALL_txt", pib, covid)


################################################
# COMBINAISON DES RÉSULTATS
################################################

metrics_recap_final <- bind_rows(
  # BDF
  metrics_BDF_txt, metrics_BDF_txtrol, metrics_BDF_txtO, metrics_BDF_txtTS, metrics_BDF_TS,
  metrics_BDF_txtFR, 
  
  # INSEE
  metrics_INSEE_txt, metrics_INSEE_notxt, metrics_INSEE_txtrol, metrics_INSEE_txtO, metrics_INSEE_txtTS, metrics_INSEE_TS,
  metrics_INSEE_txtFR, 
  
  # ECO
  metrics_all_txt
)

