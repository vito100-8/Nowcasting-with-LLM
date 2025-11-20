# Calcul MAE et RMSE de ISMA et AR(2)

source("Modèle_ISMA.R")
source("LLM_AR.R")



#################################
# Calcul MAE/RMSE
########################


metrics_ISMA <- df_ISMA |>
  summarise(
    Model = "ISMA",
    MAE_M1 = mean(abs(forecast_M1 - PIB_PR), na.rm = TRUE),
    MAE_M2 = mean(abs(forecast_M2 - PIB_PR), na.rm = TRUE),
    MAE_M3 = mean(abs(forecast_M3 - PIB_PR), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((forecast_M1 - PIB_PR)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((forecast_M2 - PIB_PR)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((forecast_M3 - PIB_PR)^2, na.rm = TRUE))
  )

metrics_AR <- df_AR |>
  summarise(
    Model = "AR(2)",
    # Un seul MAE et RMSE
    Global_MAE = mean(abs(forecast_AR - PIB_PR), na.rm = TRUE),
    Global_RMSE = sqrt(mean((forecast_AR - PIB_PR)^2, na.rm = TRUE))
  ) |>
  mutate(
    # Répétition pour chaque mois car prévision trimestrielle
    MAE_M1 = Global_MAE, MAE_M2 = Global_MAE, MAE_M3 = Global_MAE,
    RMSE_M1 = Global_RMSE, RMSE_M2 = Global_RMSE, RMSE_M3 = Global_RMSE
  ) |>
  select(-Global_MAE, -Global_RMSE)

##############################
#Tableau recap
##################


econ_metrics_table <- bind_rows(metrics_ISMA, metrics_AR) |>
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
