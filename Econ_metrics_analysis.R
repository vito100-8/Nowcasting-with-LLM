# Calcul MAE et RMSE de ISMA et AR(2)

source("Library_Nowcasting_LLM.R")
source("Modèle_ISMA.R")
source("LLM_AR.R")
source("LLM_sent_ISMA.R")

##################
# COVID
###############

COVID <- 0 # 1 = on supprime la période COVID; 0 = on garde
if (COVID == 1){
  df_ISMA <- df_ISMA |>
    filter(dates <  "2020-02-01" | dates >= "2022-02-01") 
  df_AR <- df_AR |>
    filter(dates < "2020-02-01" | dates >= "2022-02-01") 
  df_ISMA_sent <- df_ISMA_sent |>
    filter(dates < "2020-02-01" | dates >= "2022-02-01")
  df_AR_Sent <- df_AR_Sent |> 
    filter(dates < "2020-02-01" | dates >= "2022-02-01")

}

#################################
# Calcul MAE/RMSE
#################################


metrics_ISMA <- df_ISMA |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "ISMA",
    MAE_M1 = mean(abs(PIB_PR - forecast_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - forecast_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - forecast_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - forecast_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - forecast_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - forecast_M3)^2, na.rm = TRUE))
  )

metrics_AR <- df_AR |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "AR(2)",
    # Un seul MAE et RMSE
    Global_MAE = mean(abs(PIB_PR - forecast_AR), na.rm = TRUE),
    Global_RMSE = sqrt(mean((PIB_PR - forecast_AR)^2, na.rm = TRUE))
  ) |>
  mutate(
    # Répétition pour chaque mois car prévision trimestrielle
    MAE_M1 = Global_MAE, MAE_M2 = Global_MAE, MAE_M3 = Global_MAE,
    RMSE_M1 = Global_RMSE, RMSE_M2 = Global_RMSE, RMSE_M3 = Global_RMSE
  ) |>
  select(-Global_MAE, -Global_RMSE)


metrics_Sent_ISMA <- df_ISMA_sent |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Sentiment ISMA",
    MAE_M1 = mean(abs(PIB_PR - forecast_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - forecast_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - forecast_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - forecast_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - forecast_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - forecast_M3)^2, na.rm = TRUE))
  )
  
metrics_AR_Sent <- df_AR_Sent |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Sentiment AR(2)",
    MAE_M1 = mean(abs(PIB_PR - forecast_AR_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - forecast_AR_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - forecast_AR_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - forecast_AR_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - forecast_AR_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - forecast_AR_M3)^2, na.rm = TRUE))
  )


##############################
#Tableau recap
##################


econ_metrics_table <- bind_rows(metrics_ISMA, metrics_AR, metrics_Sent_ISMA) |>
  pivot_longer(
    cols = -Model, 
    names_to = c(".value", "Mois"),
    names_pattern = "(MAE|RMSE)_M([1-3])", 
  ) |>
  pivot_wider(
    names_from = Mois, 
    values_from = c(MAE, RMSE),
    names_glue = "{.value}_M{Mois}"
  ) |>
  select(Model, starts_with("MAE"), starts_with("RMSE"))




###################################
# Calcul MAE/RMSE en période
#################################

# cutoff
cutoff_date <- as.Date("2020-02-01")

#ISMA

metrics_ISMA_period <- df_ISMA |>
  filter(dates >= "2015-02-01") |>
  mutate(
    Period = case_when(
      dates < cutoff_date ~ "2015_2019",
      dates >= cutoff_date ~ "2020_2025"
    )
  ) |>
  group_by(Period) |>
  summarise(
    Model = "ISMA",
    # Calcul MAE
    MAE_M1 = mean(abs(PIB_PR - forecast_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - forecast_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - forecast_M3), na.rm = TRUE),
    # Calcul RMSE
    RMSE_M1 = sqrt(mean((PIB_PR - forecast_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - forecast_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - forecast_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

# AR

metrics_AR_period <- df_AR |>
  filter(dates >= "2015-02-01") |>
  mutate(
    Period = case_when(
      dates < cutoff_date ~ "2015_2019",
      dates >= cutoff_date ~ "2020_2025"
    )
  ) |>
  group_by(Period) |>
  summarise(
    Model = "AR(2)",
    # Calcul MAE (unique pour l'AR)
    Global_MAE = mean(abs(PIB_PR - forecast_AR), na.rm = TRUE),
    # Calcul RMSE (unique pour l'AR)
    Global_RMSE = sqrt(mean((PIB_PR - forecast_AR)^2, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    MAE_M1 = Global_MAE, MAE_M2 = Global_MAE, MAE_M3 = Global_MAE,
    RMSE_M1 = Global_RMSE, RMSE_M2 = Global_RMSE, RMSE_M3 = Global_RMSE
  ) |>
  select(-Global_MAE, -Global_RMSE)


#Sentiment ISMA


metrics_Sent_ISMA_period <- df_ISMA_sent |>
  filter(dates >= "2015-02-01") |>
  mutate(
    Period = case_when(
      dates < cutoff_date ~ "2015_2019",
      dates >= cutoff_date ~ "2020_2025"
    )
  ) |>
  group_by(Period) |>
  summarise(
    Model = "Sentiment ISMA",
    # Calcul MAE
    MAE_M1 = mean(abs(PIB_PR - forecast_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - forecast_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - forecast_M3), na.rm = TRUE),
    # Calcul RMSE
    RMSE_M1 = sqrt(mean((PIB_PR - forecast_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - forecast_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - forecast_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

metrics_AR_Sent_period <- df_AR_Sent |>
  filter(dates >= "2015-02-01") |>
  mutate(
    Period = case_when(
      dates < cutoff_date ~ "2015_2019",
      dates >= cutoff_date ~ "2020_2025"
    )
  ) |>
  group_by(Period) |>
  summarise(
    Model = "Sentiment AR(2)",
    MAE_M1 = mean(abs(PIB_PR - forecast_AR_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - forecast_AR_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - forecast_AR_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - forecast_AR_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - forecast_AR_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - forecast_AR_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

#Tableau recap
econ_metrics_period <- bind_rows(metrics_ISMA_period, metrics_AR_period, metrics_Sent_ISMA_period, metrics_AR_Sent_period )


