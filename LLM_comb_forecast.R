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
INSEE_Text <- read_xlsx("Final_Results/INSEE_Text_2020.xlsx")
INSEE_Text <- INSEE_Text |>
  rowwise() |>
  mutate(Median_forecast = median(c_across(starts_with("forecast")), na.rm = TRUE),
         .keep = "none")



pib <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")
pib <- pib |> filter(dates >= "2015-02-01")
pib_vec <- as.vector(pib$PIB_PR)
pib_vec <- rep(pib_vec, each = 3)

##Combinaison simple
Text_avg_comb <- weight_avg(BDF_Text$Median_forecast, INSEE_Text$Median_forecast)


##Combinaison inverse MSE
pred_BDF_vec <- BDF_Text$Median_forecast[1:129]
pred_INSEE_vec <- INSEE_Text$Median_forecast[1:129]
Text_MSE_comb <- rolling_inversed_weight(pib_vec, pred_BDF_vec, pred_INSEE_vec, 40)


## Combinaison à la Granger, Ramanathan
df_models <- data.frame(cbind(pred_BDF_vec, pred_INSEE_vec))
Text_gr <- gr_rolling(pib_vec, df_models, 40 )



## Comparaison des performances des trois modèles 
gr_model_vec <- as.vector(Text_gr$forecast_comb)
MSE_model_vec <- as.vector(Text_MSE_comb$nowcast)
Avg_model_vec <- Text_avg_comb[40:129]
pib_comp_vec <- as.vector(pib$PIB_PR[14:43])
BDF_Text_vec <- as.vector(BDF_Text$Median_forecast[40:129])
INSEE_Text_vec <- as.vector(INSEE_Text$Median_forecast[40:129])

#Arranger par mois dans trimestre
gr_model_def<- as.data.frame(
  matrix(
    gr_model_vec, 
    ncol = 3, 
    byrow = TRUE))
MSE_model_def <- as.data.frame(
  matrix(
    MSE_model_vec, 
    ncol = 3, 
    byrow = TRUE))
Avg_model_def <- as.data.frame(
  matrix(
    Avg_model_vec, 
    ncol = 3, 
    byrow = TRUE))
BDF_Text_def<- as.data.frame(
  matrix(
    BDF_Text_vec, 
    ncol = 3, 
    byrow = TRUE))
INSEE_Text_def <- as.data.frame(
  matrix(
    INSEE_Text_vec, 
    ncol = 3, 
    byrow = TRUE))

results_comparison <- data.frame(
  Methode = c("Moyenne Simple ", "Inverse MSE", "Granger-Ramanathan ", "BDF_Text", "INSEE_Text"),
  
  # RMSE PAR MOIS
  RMSE_Mois_1 = c(
    sqrt(mean((pib_comp_vec - Avg_model_def$V1)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - MSE_model_def$V1)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - gr_model_def$V1)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - BDF_Text_def$V1)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - INSEE_Text_def$V1)^2, na.rm = TRUE))
  ),
  
  RMSE_Mois_2 = c(
    sqrt(mean((pib_comp_vec - Avg_model_def$V2)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - MSE_model_def$V2)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - gr_model_def$V2)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - BDF_Text_def$V2)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - INSEE_Text_def$V2)^2, na.rm = TRUE))
  ),
  
  RMSE_Mois_3 = c(
    sqrt(mean((pib_comp_vec - Avg_model_def$V3)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - MSE_model_def$V3)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - gr_model_def$V3)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - BDF_Text_def$V3)^2, na.rm = TRUE)),
    sqrt(mean((pib_comp_vec - INSEE_Text_def$V3)^2, na.rm = TRUE))
  ),
  
  # MAE PAR MOIS
  MAE_Mois_1 = c(
    mean(abs(pib_comp_vec - Avg_model_def$V1), na.rm = TRUE),
    mean(abs(pib_comp_vec - MSE_model_def$V1), na.rm = TRUE),
    mean(abs(pib_comp_vec - gr_model_def$V1), na.rm = TRUE),
    mean(abs(pib_comp_vec - BDF_Text_def$V1), na.rm = TRUE),
    mean(abs(pib_comp_vec - INSEE_Text_def$V1), na.rm = TRUE)
  ),
  
  MAE_Mois_2 = c(
    mean(abs(pib_comp_vec - Avg_model_def$V2), na.rm = TRUE),
    mean(abs(pib_comp_vec - MSE_model_def$V2), na.rm = TRUE),
    mean(abs(pib_comp_vec - gr_model_def$V2), na.rm = TRUE),
    mean(abs(pib_comp_vec - BDF_Text_def$V2), na.rm = TRUE),
    mean(abs(pib_comp_vec - INSEE_Text_def$V2), na.rm = TRUE)
  ),
  
  MAE_Mois_3 = c(
    mean(abs(pib_comp_vec - Avg_model_def$V3), na.rm = TRUE),
    mean(abs(pib_comp_vec - MSE_model_def$V3), na.rm = TRUE),
    mean(abs(pib_comp_vec - gr_model_def$V3), na.rm = TRUE),
    mean(abs(pib_comp_vec - BDF_Text_def$V3), na.rm = TRUE),
    mean(abs(pib_comp_vec - INSEE_Text_def$V3), na.rm = TRUE)
  )
)

results_comparison_final <- results_comparison |>
  arrange(RMSE_Mois_3)


################################################################################
#LLM/Econométrie
################################################################################

# BDF TEXT et ISMA (par exemple)

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

pib <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")
pib <- pib |> filter(dates >= "2015-02-01")
pib_vec <- as.vector(pib$PIB_PR)



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


