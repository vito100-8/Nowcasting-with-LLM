# Script pour calculer le MAE et RMSE des modèles

source("Library_Nowcasting_LLM.R")


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


# Fonction pour enlever obs covid
filter_covid_dates <- function(df, dummy) {
  if (dummy == 0) {
    df |> 
      filter(!(Date >= "2020-02-01" & Date < "2022-02-01"))
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

# Fonction pour calculer les metrics selon covid à retirer ou non
calculate_all_models <- function(covid_dummy, pib_data) {
  
  # --- Modèles BDF ---
  m_BDF_txt    <- process_model("Final_results/BDF_text_2020.xlsx", "BDF_txt", pib_data, covid_dummy)
  m_BDF_txtrol <- process_model("Final_results/BDF_rolling_text_2020.xlsx", "BDF_txtrol", pib_data, covid_dummy)
  m_BDF_txtO   <- process_model("Final_results/BDF_just_text_2020.xlsx", "BDF_txtO", pib_data, covid_dummy)
  m_BDF_txtTS  <- process_model("Final_results/BDF_all_2020.xlsx", "BDF_txtTS", pib_data, covid_dummy)
  m_BDF_TS     <- process_model("Final_results/BDF_excel_2020.xlsx", "BDF_TS", pib_data, covid_dummy)
  m_BDF_txtFR  <- process_model("Final_results/BDF_text_FR_2020.xlsx", "BDF_txtFR", pib_data, covid_dummy)
  
  # --- Modèles INSEE ---
  m_INSEE_txt    <- process_model("Final_results/INSEE_text_2020.xlsx", "INSEE_txt", pib_data, covid_dummy)
  m_INSEE_txtrol <- process_model("Final_results/INSEE_rolling_text_2020.xlsx", "INSEE_txtrol", pib_data, covid_dummy)
  m_INSEE_txtO   <- process_model("Final_results/INSEE_just_text_2020.xlsx", "INSEE_txtO", pib_data, covid_dummy)
  m_INSEE_txtTS  <- process_model("Final_results/INSEE_all_2020.xlsx", "INSEE_txtTS", pib_data, covid_dummy)
  m_INSEE_TS     <- process_model("Final_results/INSEE_excel_2020.xlsx", "INSEE_TS", pib_data, covid_dummy)
  m_INSEE_txtFR  <- process_model("Final_results/INSEE_text_FR_2020.xlsx", "INSEE_txtFR", pib_data, covid_dummy)
  
  # --- Modèles ECO ---
  m_ALL_txt <- process_model("Final_results/ECO_text_2020.xlsx", "ALL_txt", pib_data, covid_dummy)
  
  # --- Combinaison ---
  metrics_recap <- bind_rows(
    m_BDF_txt, m_BDF_txtrol, m_BDF_txtO, m_BDF_txtTS, m_BDF_TS, m_BDF_txtFR,
    m_INSEE_txt, m_INSEE_txtrol, m_INSEE_txtO, m_INSEE_txtTS, m_INSEE_TS, m_INSEE_txtFR,
    m_ALL_txt
  )
  
  return(metrics_recap)
}

################################################
# EXÉCUTION ET SAUVEGARDE DES DEUX SCÉNARIOS
################################################

#With covid
metrics_covid <- calculate_all_models(covid_dummy = 1, pib_data = pib)
write.xlsx(metrics_covid, "metrics_covid.xlsx")

#Without covid
metrics_no_covid <- calculate_all_models(covid_dummy = 0, pib_data = pib)
write.xlsx(metrics_no_covid, "metrics_no_covid.xlsx")

#Si jamais appelé dans un autre script on donne la valeur avec le covid à la variable
metrics_recap_final <- metrics_covid