# Calcul MAE et RMSE de ISMA et AR(2)

source("LLM_Sentiment_AR.R")
source("LLM_AR_climat.R")

##################
# COVID
###############

COVID <- 0 # 1 = on supprime la période COVID; 0 = on garde
if (COVID == 1){
  df_AR_Clim <- Results_Forecast_Clim |> 
    filter(dates < "2020-02-01" | dates >= "2022-02-01")
  df_AR_Sent <- Resuls_Forecast_Sent |> 
    filter(dates < "2020-02-01" | dates >= "2022-02-01")
}else {
  df_AR_Clim <- Results_Forecast_Clim
  df_AR_Sent <- Results_Forecast_Sent 
  
}

#################################
# Calcul MAE/RMSE
#################################



# Indice de sentiment BDF 
metrics_BDF_AR_Sent <- df_AR_Sent |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Sentiment BDF",
    MAE_M1 = mean(abs(PIB_PR - Prev_BDF_SENT_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - Prev_BDF_SENT_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - Prev_BDF_SENT_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M3)^2, na.rm = TRUE))
  )

#Indice de sentiment INSEE
metrics_INSEE_AR_Sent <- df_AR_Sent |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Sentiment INSEE",
    MAE_M1 = mean(abs(PIB_PR - Prev_INSEE_SENT_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - Prev_INSEE_SENT_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - Prev_INSEE_SENT_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M3)^2, na.rm = TRUE))
  )



# BDF Modèle 1 (Industrie Seule)
metrics_AR_Clim_BDF_1 <- df_AR_Clim |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat BDF (Ind)",
    MAE_M1 = mean(abs(PIB_PR - BDF_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - BDF_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - BDF_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - BDF_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - BDF_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - BDF_IND_M3)^2, na.rm = TRUE))
  )

#  BDF Modèle 2 (Industrie + Services)
metrics_AR_Clim_BDF_2 <- df_AR_Clim |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat BDF (All)",
    MAE_M1 = mean(abs(PIB_PR - BDF_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - BDF_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - BDF_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - BDF_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - BDF_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - BDF_ALL_M3)^2, na.rm = TRUE))
  )

# INSEE Modèle 1 (Industrie Seule)
metrics_Clim_INSEE_1 <- df_AR_Clim |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat INSEE (Ind)",
    MAE_M1 = mean(abs(PIB_PR - INSEE_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - INSEE_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - INSEE_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - INSEE_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - INSEE_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - INSEE_IND_M3)^2, na.rm = TRUE))
  )

#  INSEE Modèle 2 (Industrie + Services + Bâtiment)
metrics_Clim_INSEE_2 <- df_AR_Clim |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat INSEE (All)",
    MAE_M1 = mean(abs(PIB_PR - INSEE_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - INSEE_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - INSEE_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - INSEE_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - INSEE_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - INSEE_ALL_M3)^2, na.rm = TRUE))
  )

#INSEE ET BDF Industrie
metrics_Clim_comb_1 <- df_AR_Clim|>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat INSEE/BDF (IND)",
    MAE_M1 = mean(abs(PIB_PR - COMB_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - COMB_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - COMB_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - COMB_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - COMB_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - COMB_IND_M3)^2, na.rm = TRUE))
  )


#INSEE et BDF tous les climats
metrics_Clim_comb_2 <- df_AR_Clim |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat INSEE/BDF (All)",
    MAE_M1 = mean(abs(PIB_PR - COMB_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - COMB_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - COMB_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - COMB_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - COMB_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - COMB_ALL_M3)^2, na.rm = TRUE))
  )


##############################
#Tableau recap
##################


econ_metrics_table <- bind_rows( metrics_BDF_AR_Sent, metrics_INSEE_AR_Sent, metrics_AR_Clim_BDF_1, 
                                metrics_AR_Clim_BDF_2, metrics_Clim_INSEE_1, metrics_Clim_INSEE_2 ,
                                metrics_Clim_comb_1, metrics_Clim_comb_2) |>
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

#Indice de Sentiment BDF
metrics_Sent_BDF_period <- df_AR_Sent |>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Sentiment BDF ",
    MAE_M1 = mean(abs(PIB_PR - Prev_BDF_SENT_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - Prev_BDF_SENT_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - Prev_BDF_SENT_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

#Indice de sentiment INSEE
metrics_Sent_INSEE_period <- df_AR_Sent |>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Sentiment INSEE",
    MAE_M1 = mean(abs(PIB_PR - Prev_INSEE_SENT_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - Prev_INSEE_SENT_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - Prev_INSEE_SENT_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )


# Climat BDF Modèle 1 (Industrie)
metrics_Clim_BDF_1_period <- df_AR_Clim |>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat BDF (Ind)",
    MAE_M1 = mean(abs(PIB_PR - BDF_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - BDF_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - BDF_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - BDF_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - BDF_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - BDF_IND_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

# Climat BDF Modèle 2 (Industrie + Services)
metrics_Clim_BDF_2_period <- df_AR_Clim|>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat BDF (All)",
    MAE_M1 = mean(abs(PIB_PR - BDF_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - BDF_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - BDF_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - BDF_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - BDF_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - BDF_ALL_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

# Climat INSEE Modèle 1 (Industrie)
metrics_Clim_INSEE_1_period <- df_AR_Clim|>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat INSEE (Ind)",
    MAE_M1 = mean(abs(PIB_PR - INSEE_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - INSEE_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - INSEE_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - INSEE_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - INSEE_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - INSEE_IND_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

# Climat INSEE Modèle 2 (Tout)
metrics_Clim_INSEE_2_period <- df_AR_Clim|>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat INSEE (All)",
    MAE_M1 = mean(abs(PIB_PR - INSEE_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - INSEE_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - INSEE_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - INSEE_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - INSEE_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - INSEE_ALL_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

#INSEE/BDF INDUSTRIE
metrics_Clim_comb_1_period <- df_AR_Clim|>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat INSEE (All)",
    MAE_M1 = mean(abs(PIB_PR - COMB_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - COMB_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - COMB_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - COMB_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - COMB_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - COMB_IND_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

#INSEE/BDF ALL
metrics_Clim_comb_2_period <- df_AR_Clim |>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat INSEE (All)",
    MAE_M1 = mean(abs(PIB_PR - COMB_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - COMB_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - COMB_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - COMB_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - COMB_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - COMB_ALL_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )


#TABLEAU RECAP FINAL
econ_metrics_period <- bind_rows(
  metrics_Sent_BDF_period,
  metrics_Sent_INSEE_period,
  metrics_Clim_BDF_1_period,
  metrics_Clim_BDF_2_period,
  metrics_Clim_INSEE_1_period,
  metrics_Clim_INSEE_2_period,
  metrics_Clim_comb_1_period,
  metrics_Clim_comb_2_period
  
)