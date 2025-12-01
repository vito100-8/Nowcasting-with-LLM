# SCRIPT : FORECAST COMBINATION

# BUT : proposer plusieurs combinaison de forecasts entre modèles (LLM et/ou économétrique)

#rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Modèle_ISMA.R")
source('LLM_AR.R')

################################################################################
# ONLY LLM models
################################################################################

#################
# BDF/INSEE Comb
#################


# Text (par exemple, modifiable) : 



BDF_Text <- read_xlsx("Final_Results/BDF_Text_2020.xlsx")
BDF_Text <- BDF_Text |>
  rowwise() |>
  mutate(Median_forecast = median(c_across(starts_with("forecast")), na.rm = TRUE),
         .keep = "none")

BDF_Text_def <-  as.data.frame(
  matrix(
    BDF_Text$Median_forecast, 
    ncol = 3, 
    byrow = TRUE))
BDF_Text_def <- BDF_Text_def[1:43,]


INSEE_Text <- read_xlsx("Final_Results/INSEE_Text_2020.xlsx")
INSEE_Text <- INSEE_Text |>
  rowwise() |>
  mutate(Median_forecast = median(c_across(starts_with("forecast")), na.rm = TRUE),
         .keep = "none")

INSEE_Text_def <-  as.data.frame(
  matrix(
    INSEE_Text$Median_forecast, 
    ncol = 3, 
    byrow = TRUE))
INSEE_Text_def <- INSEE_Text_def[1:43,]


pib <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")
pib <- pib |> filter(dates >= "2015-02-01")
pib_vec <- as.vector(pib$PIB_PR)


#Lancement modèles
comb_avg <- weight_avg_month(BDF_Text_def, INSEE_Text_def)

comb_MSE <- rolling_inversed_weight_month(pib_vec, BDF_Text_def, INSEE_Text_def, 10)

comb_gr <- gr_rolling_month(pib_vec, BDF_Text_def, INSEE_Text_def, 10)

#Uniformisation pour pouvoir comparer les résultats
pib_vec_window <- pib_vec[10:43]
comb_avg <- comb_avg[10:43,]
BDF_Text_window <- BDF_Text_def[10:43,]
INSEE_Text_def <- INSEE_Text_def[10:43,]

#write.xlsx(cbind(pib_vec, comb_avg, BDF_Text_def, INSEE_Text_def, comb_gr$forecast_comb, comb_MSE$nowcast), "Comb_forecast_LLM.xlsx")

#df_res <- read_xlsx("Comb_forecast_LLM.xlsx")
#refaire tableau avec ces valeurs


#Résultats
results_comparison <- data.frame(
  Methode = c("Moyenne Simple ", "Inverse MSE", "Granger-Ramanathan ", "BDF_Text", "INSEE_Text"),
  
  # RMSE PAR MOIS
  RMSE_Mois_1 = c(
    sqrt(mean((pib_vec_window - comb_avg$Mois_1)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - comb_MSE$nowcast$Mois_1)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - comb_gr$forecast_comb$Mois_1)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - BDF_Text_window$V1)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - INSEE_Text_def$V1)^2, na.rm = TRUE))
  ),
  
  RMSE_Mois_2 = c(
    sqrt(mean((pib_vec_window - comb_avg$Mois_2)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - comb_MSE$nowcast$Mois_2)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - comb_gr$forecast_comb$Mois_2)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - BDF_Text_window$V2)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - INSEE_Text_def$V2)^2, na.rm = TRUE))
  ),
  
  RMSE_Mois_3 = c(
    sqrt(mean((pib_vec_window - comb_avg$Mois_3)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window -comb_MSE$nowcast$Mois_3)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - comb_gr$forecast_comb$Mois_3)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - BDF_Text_window$V3)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_window - INSEE_Text_def$V3)^2, na.rm = TRUE))
  ),
  
  # MAE PAR MOIS
  MAE_Mois_1 = c(
    mean(abs(pib_vec_window - comb_avg$Mois_1), na.rm = TRUE),
    mean(abs(pib_vec_window - comb_MSE$nowcast$Mois_1), na.rm = TRUE),
    mean(abs(pib_vec_window - comb_gr$forecast_comb$Mois_1), na.rm = TRUE),
    mean(abs(pib_vec_window - BDF_Text_window$V1), na.rm = TRUE),
    mean(abs(pib_vec_window - INSEE_Text_def$V1), na.rm = TRUE)
  ),
  
  MAE_Mois_2 = c(
    mean(abs(pib_vec_window - comb_avg$Mois_2), na.rm = TRUE),
    mean(abs(pib_vec_window -comb_MSE$nowcast$Mois_2), na.rm = TRUE),
    mean(abs(pib_vec_window - comb_gr$forecast_comb$Mois_2), na.rm = TRUE),
    mean(abs(pib_vec_window - BDF_Text_window$V2), na.rm = TRUE),
    mean(abs(pib_vec_window - INSEE_Text_def$V2), na.rm = TRUE)
  ),
  
  MAE_Mois_3 = c(
    mean(abs(pib_vec_window - comb_avg$Mois_3), na.rm = TRUE),
    mean(abs(pib_vec_window - comb_MSE$nowcast$Mois_3), na.rm = TRUE),
    mean(abs(pib_vec_window - comb_gr$forecast_comb$Mois_3), na.rm = TRUE),
    mean(abs(pib_vec_window - BDF_Text_window$V3), na.rm = TRUE),
    mean(abs(pib_vec_window - INSEE_Text_def$V3), na.rm = TRUE)
  )
)

results_comparison_final_llm <- results_comparison |>
  arrange(RMSE_Mois_3)



################################################################################
#LLM/Econométrie
################################################################################

# BDF TEXT et ISMA (par exemple)

df_ISMA_prev <- df_ISMA |>
  select(forecast_M1:forecast_M3)
  

#Lancement modèles
comb_avg <- weight_avg_month(BDF_Text_def, df_ISMA_prev)

comb_MSE <- rolling_inversed_weight_month(pib_vec, BDF_Text_def, df_ISMA_prev, 10)

comb_gr <- gr_rolling_month(pib_vec, BDF_Text_def, df_ISMA_prev, 10)

#Uniformisation pour pouvoir comparer les résultats
pib_vec <- pib_vec[10:43]
comb_avg <- comb_avg[10:43,]
BDF_Text_def <- BDF_Text_def[10:43,]
df_ISMA_prev <- df_ISMA_prev[10:43,]

#Résultats
results_comparison <- data.frame(
Methode = c("Moyenne Simple ", "Inverse MSE", "Granger-Ramanathan ", "BDF_Text", "ISMA"),

# RMSE PAR MOIS
RMSE_Mois_1 = c(
  sqrt(mean((pib_vec - comb_avg$Mois_1)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - comb_MSE$nowcast$Mois_1)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - comb_gr$forecast_comb$Mois_1)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - BDF_Text_def$V1)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - df_ISMA_prev$forecast_M1)^2, na.rm = TRUE))
),

RMSE_Mois_2 = c(
  sqrt(mean((pib_vec - comb_avg$Mois_2)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - comb_MSE$nowcast$Mois_2)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - comb_gr$forecast_comb$Mois_2)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - BDF_Text_def$V2)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - df_ISMA_prev$forecast_M2)^2, na.rm = TRUE))
),

RMSE_Mois_3 = c(
  sqrt(mean((pib_vec - comb_avg$Mois_3)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec -comb_MSE$nowcast$Mois_3)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - comb_gr$forecast_comb$Mois_3)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - BDF_Text_def$V3)^2, na.rm = TRUE)),
  sqrt(mean((pib_vec - df_ISMA_prev$forecast_M3)^2, na.rm = TRUE))
),

# MAE PAR MOIS
MAE_Mois_1 = c(
  mean(abs(pib_vec - comb_avg$Mois_1), na.rm = TRUE),
  mean(abs(pib_vec - comb_MSE$nowcast$Mois_1), na.rm = TRUE),
  mean(abs(pib_vec - comb_gr$forecast_comb$Mois_1), na.rm = TRUE),
  mean(abs(pib_vec - BDF_Text_def$V1), na.rm = TRUE),
  mean(abs(pib_vec - df_ISMA_prev$forecast_M1), na.rm = TRUE)
),

MAE_Mois_2 = c(
  mean(abs(pib_vec - comb_avg$Mois_2), na.rm = TRUE),
  mean(abs(pib_vec -comb_MSE$nowcast$Mois_2), na.rm = TRUE),
  mean(abs(pib_vec - comb_gr$forecast_comb$Mois_2), na.rm = TRUE),
  mean(abs(pib_vec - BDF_Text_def$V2), na.rm = TRUE),
  mean(abs(pib_vec - df_ISMA_prev$forecast_M2), na.rm = TRUE)
),

MAE_Mois_3 = c(
  mean(abs(pib_vec - comb_avg$Mois_3), na.rm = TRUE),
  mean(abs(pib_vec - comb_MSE$nowcast$Mois_3), na.rm = TRUE),
  mean(abs(pib_vec - comb_gr$forecast_comb$Mois_3), na.rm = TRUE),
  mean(abs(pib_vec - BDF_Text_def$V3), na.rm = TRUE),
  mean(abs(pib_vec - df_ISMA_prev$forecast_M3), na.rm = TRUE)
  )
)

results_comparison_final <- results_comparison |>
  arrange(RMSE_Mois_3)




##############################################
#Correction
#############################################


# Configuration
y_target    <- pib$PIB_PR
dates       <- pib$dates
d_start     <- as.Date("2020-01-01")
d_end       <- as.Date("2021-12-31")
window      <- 8
mes_modeles <- list(BDF = BDF_Text_def, ISMA = df_ISMA_prev) 

# Exécuter les modèles 

## Moyenne Simple
res_avg <- simple_avg_month_v2(mes_modeles)
res_avg<- res_avg[(window + 1): 43,]

## Inverse MSE 
out_inv <- rolling_inversed_weight_month_v2(y_target, mes_modeles, dates, d_start, d_end, window)
res_inv <- out_inv$nowcast

## Granger-Ramanathan 
out_gr <- gr_rolling_month_v2(y_target, mes_modeles, dates, d_start, d_end, window)

res_gr  <- out_gr$forecast_comb

# CalculMAE/RMSE

## Fonction utilitaire pour calculer les erreurs
calc_errors <- function(obs, preds) {
  err <- preds - obs
  c(MAE = mean(abs(err)), RMSE = sqrt(mean(err^2)))
}
metrics_list <- list()

  
for (j in 1:3) {
  col_name <- paste0("Mois_", j)
  df_eval <- data.frame(
    Obs   = y_target[(window + 1): 43],
    BDF   = mes_modeles$BDF[(window + 1): 43, j],
    ISMA = mes_modeles$ISMA[(window + 1): 43, j],
    AVG   = res_avg[, j],
    INV   = res_inv[, j],
    GR    = res_gr[, j]
  )
  
  df_eval <- na.omit(df_eval)
  res_horizon <- sapply(df_eval[, -1], function(preds) calc_errors(df_eval$Obs, preds))
  metrics_list[[col_name]] <- res_horizon
}

