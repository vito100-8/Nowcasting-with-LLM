#Script pour calculer le MAE et RMSE des modèles

rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")


################################################
# PRÉPARATION DES DONNÉES DE PIB
################################################

df_PIB <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")

# Nettoyage du PIB 
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
# CALCUL MAE/RMSE PAR MODÈLE 
################################################

# --- Modèles BDF  ---

# =========================================================
# MODÈLE 1 : BDF_text
# =========================================================

df_BDF_text <- read_xlsx("Final_results/BDF_text_2020.xlsx") 

df_BDF_text_long <- df_BDF_text |>
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

df_BDF_text_wide <- df_BDF_text_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

BDF_text_forecast <- left_join(df_BDF_text_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_BDF_text <- BDF_text_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "BDF_text",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))

# =========================================================
# MODÈLE 2 : BDF_noText
# =========================================================

df_BDF_noText <- read_xlsx("Final_results/BDF_noText_2020.xlsx") 

df_BDF_noText_long <- df_BDF_noText |>
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

df_BDF_noText_wide <- df_BDF_noText_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

BDF_noText_forecast <- left_join(df_BDF_noText_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_BDF_noText <- BDF_noText_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "BDF_noText",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))

# =========================================================
# MODÈLE 3 : BDF_rolling_text
# =========================================================

df_BDF_rolling_text <- read_xlsx("Final_results/BDF_rolling_text_2020.xlsx") 

df_BDF_rolling_text_long <- df_BDF_rolling_text |>
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

df_BDF_rolling_text_wide <- df_BDF_rolling_text_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

BDF_rolling_text_forecast <- left_join(df_BDF_rolling_text_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_BDF_rolling_text <- BDF_rolling_text_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "BDF_rolling_text",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))

# =========================================================
# MODÈLE 4 : BDF_just_text
# =========================================================

df_BDF_just_text <- read_xlsx("Final_results/BDF_just_text_2020.xlsx") 

df_BDF_just_text_long <- df_BDF_just_text |>
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

df_BDF_just_text_wide <- df_BDF_just_text_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

BDF_just_text_forecast <- left_join(df_BDF_just_text_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_BDF_just_text <- BDF_just_text_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "BDF_just_text",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))

# =========================================================
# MODÈLE 5 : BDF_all
# =========================================================

df_BDF_all <- read_xlsx("Final_results/BDF_all_2020.xlsx") 

df_BDF_all_long <- df_BDF_all |>
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

df_BDF_all_wide <- df_BDF_all_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

BDF_all_forecast <- left_join(df_BDF_all_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_BDF_all <- BDF_all_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "BDF_all",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))
# =========================================================
# MODÈLE 6 : BDF_Excel
# =========================================================

df_BDF_excel <- read_xlsx("Final_results/BDF_excel_2020.xlsx") 

df_BDF_excel_long <- df_BDF_excel |>
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

df_BDF_excel_wide <- df_BDF_excel_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

BDF_excel_forecast <- left_join(df_BDF_excel_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_BDF_excel <- BDF_excel_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "BDF_excel",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))

# =========================================================
# MODÈLE 7 : BDF_Excel_error
# =========================================================

df_BDF_excel_error <- read_xlsx("Final_results/BDF_excel_error_2020.xlsx") 

df_BDF_excel_error_long <- df_BDF_excel_error |>
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

df_BDF_excel_error_wide <- df_BDF_excel_error_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

BDF_excel_error_forecast <- left_join(df_BDF_excel_error_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_BDF_excel_error <- BDF_excel_error_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "BDF_excel_error",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))


# --- Modèles INSEE ---

# =========================================================
# MODÈLE 8 : INSEE_text
# =========================================================

df_INSEE_text <- read_xlsx("Final_results/INSEE_text_2020.xlsx") 

df_INSEE_text_long <- df_INSEE_text |>
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

df_INSEE_text_wide <- df_INSEE_text_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

INSEE_text_forecast <- left_join(df_INSEE_text_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_INSEE_text <- INSEE_text_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "INSEE_text",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))

# =========================================================
# MODÈLE 9 : INSEE_noText
# =========================================================

df_INSEE_noText <- read_xlsx("Final_results/INSEE_noText_2020.xlsx") 

df_INSEE_noText_long <- df_INSEE_noText |>
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

df_INSEE_noText_wide <- df_INSEE_noText_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

INSEE_noText_forecast <- left_join(df_INSEE_noText_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_INSEE_noText <- INSEE_noText_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "INSEE_noText",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))

# =========================================================
# MODÈLE 10: INSEE_rolling_text
# =========================================================

df_INSEE_rolling_text <- read_xlsx("Final_results/INSEE_rolling_text_2020.xlsx") 

df_INSEE_rolling_text_long <- df_INSEE_rolling_text |>
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

df_INSEE_rolling_text_wide <- df_INSEE_rolling_text_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

INSEE_rolling_text_forecast <- left_join(df_INSEE_rolling_text_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_INSEE_rolling_text <- INSEE_rolling_text_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "INSEE_rolling_text",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))

# =========================================================
# MODÈLE 11 : INSEE_just_text
# =========================================================

df_INSEE_just_text <- read_xlsx("Final_results/INSEE_just_text_2020.xlsx") 

df_INSEE_just_text_long <- df_INSEE_just_text |>
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

df_INSEE_just_text_wide <- df_INSEE_just_text_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

INSEE_just_text_forecast <- left_join(df_INSEE_just_text_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_INSEE_just_text <- INSEE_just_text_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "INSEE_just_text",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))

# =========================================================
# MODÈLE 12 : INSEE_all
# =========================================================

df_INSEE_all <- read_xlsx("Final_results/INSEE_all_2020.xlsx") 

df_INSEE_all_long <- df_INSEE_all |>
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

df_INSEE_all_wide <- df_INSEE_all_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

INSEE_all_forecast <- left_join(df_INSEE_all_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_INSEE_all <- INSEE_all_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "INSEE_all",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))


# =========================================================
# MODÈLE 13 : INSEE_Excel
# =========================================================

df_INSEE_excel <- read_xlsx("Final_results/INSEE_excel_2020.xlsx") 

df_INSEE_excel_long <- df_INSEE_excel |>
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

df_INSEE_excel_wide <- df_INSEE_excel_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

INSEE_excel_forecast <- left_join(df_INSEE_excel_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_INSEE_excel <- INSEE_excel_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "INSEE_excel",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))


# =========================================================
# MODÈLE 14 : INSEE_Excel_error
# =========================================================

df_INSEE_excel_error <- read_xlsx("Final_results/INSEE_excel_error_2020.xlsx") 

df_INSEE_excel_error_long <- df_INSEE_excel_error |>
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

df_INSEE_excel_error_wide <- df_INSEE_excel_error_long |>
  pivot_wider(
    id_cols = c(forecast_year, forecast_quarter), 
    names_from = month_in_quarter, values_from = median_forecast,
    names_prefix = "Forecast_Mois_"
  ) |>
  arrange(forecast_year, forecast_quarter)

INSEE_excel_error_forecast <- left_join(df_INSEE_excel_error_wide, pib, join_by(forecast_year, forecast_quarter))

metrics_INSEE_excel_error <- INSEE_excel_error_forecast |>
  filter(!is.na(PIB_PR)) |>
  summarise(Model = "INSEE_excel_error",
            MAE_Mois_1 = mean(abs(PIB_PR - Forecast_Mois_1), na.rm = TRUE),
            MAE_Mois_2 = mean(abs(PIB_PR - Forecast_Mois_2), na.rm = TRUE),
            MAE_Mois_3 = mean(abs(PIB_PR - Forecast_Mois_3), na.rm = TRUE),
            RMSE_Mois_1 = sqrt(mean((PIB_PR - Forecast_Mois_1)^2, na.rm = TRUE)),
            RMSE_Mois_2 = sqrt(mean((PIB_PR - Forecast_Mois_2)^2, na.rm = TRUE)),
            RMSE_Mois_3 = sqrt(mean((PIB_PR - Forecast_Mois_3)^2, na.rm = TRUE)))


################################################
# COMBINAISON DES RÉSULTATS
################################################

metrics_recap_final <- bind_rows(
  metrics_BDF_text, metrics_BDF_noText, metrics_BDF_rolling_text, metrics_BDF_just_text,  metrics_BDF_all,metrics_BDF_excel, metrics_BDF_excel_error,
  metrics_INSEE_text, metrics_INSEE_noText, metrics_INSEE_rolling_text, metrics_INSEE_just_text,  metrics_INSEE_all, metrics_INSEE_excel, metrics_INSEE_excel_error,
)


