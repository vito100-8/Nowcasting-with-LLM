# SCRIPT : FORECAST COMBINATION

# BUT : proposer plusieurs combinaison de forecasts entre modèles (LLM et/ou économétrique)

rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Modèle_ISMA.R")
source('LLM_AR.R')


# Text (par exemple, modifiable) : 


############################################
# Initialisation Fichiers
#########################################

BDF_Text <- read_xlsx("Final_Results/BDF_Text_2020.xlsx")

INSEE_Text <- read_xlsx("Final_Results/INSEE_Text_2020.xlsx")

BDF_2010 <- read_xlsx("Results/BDF_noText201014.xlsx")

INSEE_2010 <- read_xlsx("Results/INSEE_noText201014.xlsx")

pib <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")


#DF avec observations depuis 2010
BDF_Text<- bind_rows(BDF_2010, BDF_Text)
INSEE_Text <- bind_rows(INSEE_2010, INSEE_Text)

#####################################
#Variables globales
#################################

start_date_common <- as.Date("2015-02-01") 

# Alignement du PIB 
pib_subset <- pib |> 
  filter(dates >= start_date_common)

window      <- 20
limit_index <- 43 
y_target  <- pib_subset$PIB_PR[1:limit_index]
dates <- pib_subset$dates[1:limit_index]
d_start   <- as.Date("2020-01-01")
d_end     <- as.Date("2021-12-31")



################################################################################
# Construction résultats INSEE et BDF
################################################################################


#Préparation des df 
BDF_Text <- BDF_Text |>
  rowwise() |>
  mutate(Median_forecast = median(c_across(starts_with("forecast")), na.rm = TRUE),
         .keep = "none")

BDF_Text_def <-  as.data.frame(
  matrix(
    BDF_Text$Median_forecast, 
    ncol = 3, 
    byrow = TRUE))
BDF_Text_def <- BDF_Text_def[1:limit_index,]


INSEE_Text <- INSEE_Text |>
  rowwise() |>
  mutate(Median_forecast = median(c_across(starts_with("forecast")), na.rm = TRUE),
         .keep = "none")

INSEE_Text_def <-  as.data.frame(
  matrix(
    INSEE_Text$Median_forecast, 
    ncol = 3, 
    byrow = TRUE))
INSEE_Text_def <- INSEE_Text_def[1:limit_index,]


# Exécuter les modèles 
mes_modeles_INSEE_BDF <- list(INSEE = INSEE_Text_def,BDF = BDF_Text_def)

## Moyenne Simple
res_avg <- simple_avg_month_v2(mes_modeles_INSEE_BDF)
res_avg<- res_avg[(window + 1): limit_index,]

## Inverse MSE 
out_inv <- rolling_inversed_weight_month_v2(y_target, mes_modeles_INSEE_BDF, dates, d_start, d_end, window)
res_inv <- out_inv$nowcast

## Granger-Ramanathan 
out_gr <- gr_rolling_month_v2(y_target, mes_modeles_INSEE_BDF, dates, d_start, d_end, window)

res_gr  <- out_gr$forecast_comb

# CalculMAE/RMSE
metrics_list_INSEE_BDF <- list()


for (j in 1:3) {
  col_name <- paste0("Mois_", j)
  df_eval <- data.frame(
    Obs   = y_target[(window + 1): limit_index],
    BDF   = mes_modeles_INSEE_BDF$BDF[(window + 1): limit_index, j],
    INSEE = mes_modeles_INSEE_BDF$INSEE[(window + 1): limit_index, j],
    AVG   = res_avg[, j],
    INV   = res_inv[, j],
    GR    = res_gr[, j]
  )
  
  df_eval <- na.omit(df_eval)
  res_horizon <- sapply(df_eval[, -1], function(preds) calc_errors(df_eval$Obs, preds))
  metrics_list_INSEE_BDF[[col_name]] <- res_horizon
}
  
  

##################################
# CONSTRUCTION Résultats BDF - ISMA
#################################

#Ajout données ISMA 
df_ISMA_prev <- df_ISMA |>
    filter(dates>= "2015-02-01") |>
    select(forecast_M1:forecast_M3) 


# Configuration modèles
mes_modeles_BDF_ISMA <- list(BDF = BDF_Text_def, ISMA = df_ISMA_prev)


# Fonctions combinaisons

# Moyenne Simple
comb_avg <- simple_avg_month_v2(mes_modeles_BDF_ISMA)

# Inverse MSE 
comb_MSE <- rolling_inversed_weight_month_v2(
  y = y_target, 
  model_list = mes_modeles_BDF_ISMA, 
  dates = dates, 
  start_covid = d_start, 
  end_covid = d_end, 
  rolling_window = window
)

#  Granger-Ramanathan 

comb_gr <- gr_rolling_month_v2(
  y = y_target, 
  model_list = mes_modeles_BDF_ISMA, 
  dates = dates, 
  start_covid = d_start, 
  end_covid = d_end, 
  rolling_window = window
)


# Calcul MAE/RMSE
metrics_list_ISMA_BDF <- list()

if(nrow(comb_avg) == length(y_target)) {
  avg_aligned <- comb_avg[(window + 1):limit_index, ]
} else {
  avg_aligned <- comb_avg
}

# Boucle sur les 3 mois
for (j in 1:3) {
  col_name <- paste0("Mois_", j)
  
  df_eval <- data.frame(
    Obs            = y_target[(window + 1):limit_index],
    BDF_Text       = mes_modeles_BDF_ISMA$BDF[(window + 1):limit_index, j],
    ISMA           = mes_modeles_BDF_ISMA$ISMA[(window + 1):limit_index, j],
    Moyenne_Simple = avg_aligned[, j],
    Inverse_MSE    = comb_MSE$nowcast[, j],
    Granger_Ram    = comb_gr$forecast_comb[, j]
  )
  
  # Nettoyage des NA 
  df_eval <- na.omit(df_eval)
  
  # Application de la fonction d'erreur sur les colonnes de prévision
  res_horizon <- sapply(df_eval[, -1], function(preds) calc_errors(df_eval$Obs, preds))
  

  metrics_list_ISMA_BDF[[col_name]] <- res_horizon
}

colnames(metrics_list_ISMA_BDF$Mois_1) <- c("BDF_Text", "ISMA", "Moyenne_Simple", "Inverse_MSE", " Granger_Ram")
colnames(metrics_list_ISMA_BDF$Mois_2) <- c("BDF_Text", "ISMA", "Moyenne_Simple", "Inverse_MSE", " Granger_Ram")
colnames(metrics_list_ISMA_BDF$Mois_3) <- c("BDF_Text", "ISMA", "Moyenne_Simple", "Inverse_MSE", " Granger_Ram")



####################################################
# Modèles BDF
##################################################

#Déclaration des modèles
text <- read_xlsx("Final_Results/BDF_text_2020.xlsx")
rolling <- read_xlsx("Final_Results/BDF_rolling_text_2020.xlsx")
noText <- read_xlsx("Final_Results/BDF_noText_2020.xlsx")
all <- read_xlsx("Final_Results/BDF_all_2020.xlsx")
text_FR <- read_xlsx("Final_Results/BDF_text_FR_2020.xlsx")

#Traitemnt des données de chaque modèles
mes_modeles_BDF <- list(Text = text, NoText = noText, Rolling = rolling, Text_FR = text_FR, All = all)

for (k in 1:length(mes_modeles_BDF)){
  
  # Nettoyage des données de chaque modèle
  tmp <- mes_modeles_BDF[[k]] |>
    rowwise() |>
    mutate(Median_forecast = median(c_across(starts_with("forecast")), na.rm = TRUE),
           .keep = "none") |>
    ungroup()
  
  k_def <- as.data.frame(
    matrix(
      tmp$Median_forecast, 
      ncol = 3, 
      byrow = TRUE))
  
  # Ecraser anciens modèles par une version propre
  mes_modeles_BDF[[k]] <- k_def[1:limit_index, ]
}

# Exécuter les modèles 

## Moyenne Simple
res_avg <- simple_avg_month_v2(mes_modeles_BDF)
res_avg<- res_avg[(window + 1): limit_index,]

## Inverse MSE 
out_inv <- rolling_inversed_weight_month_v2(y_target, mes_modeles_BDF, dates, d_start, d_end, window)
res_inv <- out_inv$nowcast

## Granger-Ramanathan 
out_gr <- gr_rolling_month_v2(y_target, mes_modeles_BDF, dates, d_start, d_end, window)

res_gr  <- out_gr$forecast_comb

# Calcul MAE/RMSE
metrics_list_BDF <- list()


for (j in 1:3) {
  col_name <- paste0("Mois_", j)
  df_eval <- data.frame(
    Obs   = y_target[(window + 1): limit_index],
    Text   = mes_modeles_BDF$Text[(window + 1): limit_index, j],
    noText   = mes_modeles_BDF$NoText[(window + 1): limit_index, j],
    Rolling   = mes_modeles_BDF$Rolling[(window + 1): limit_index, j],
    All   = mes_modeles_BDF$All[(window + 1): limit_index, j],
    Text_FR   = mes_modeles_BDF$Text_FR[(window + 1): limit_index, j],
    AVG   = res_avg[, j],
    INV   = res_inv[, j],
    GR    = res_gr[, j]
  )
  
  df_eval <- na.omit(df_eval)
  res_horizon <- sapply(df_eval[, -1], function(preds) calc_errors(df_eval$Obs, preds))
  metrics_list_BDF[[col_name]] <- res_horizon
}


################################################################################
# Modèles INSEE
################################################################################

#Déclaration des modèles
text <- read_xlsx("Final_Results/INSEE_text_2020.xlsx")
rolling <- read_xlsx("Final_Results/INSEE_rolling_text_2020.xlsx")
noText <- read_xlsx("Final_Results/INSEE_noText_2020.xlsx")
all <- read_xlsx("Final_Results/INSEE_all_2020.xlsx")
text_FR <- read_xlsx("Final_Results/INSEE_text_FR_2020.xlsx")

#Traitemznt des données de chaque modèles
mes_modeles_INSEE <- list(Text = text, NoText = noText, Rolling = rolling, Text_FR = text_FR, All = all)

for (k in 1:length(mes_modeles_INSEE)){
  
  # Nettoyage des données de chaque modèle
  tmp <- mes_modeles_INSEE[[k]] |>
    rowwise() |>
    mutate(Median_forecast = median(c_across(starts_with("forecast")), na.rm = TRUE),
           .keep = "none") |>
    ungroup()
  
  k_def <- as.data.frame(
    matrix(
      tmp$Median_forecast, 
      ncol = 3, 
      byrow = TRUE))
  
  # Ecraser anciens modèles par une version propre
  mes_modeles_INSEE[[k]] <- k_def[1:limit_index, ]
}

# Exécuter les modèles 

## Moyenne Simple
res_avg <- simple_avg_month_v2(mes_modeles_INSEE)
res_avg<- res_avg[(window + 1): limit_index,]

## Inverse MSE 
out_inv <- rolling_inversed_weight_month_v2(y_target, mes_modeles_INSEE, dates, d_start, d_end, window)
res_inv <- out_inv$nowcast

## Granger-Ramanathan 
out_gr <- gr_rolling_month_v2(y_target, mes_modeles_INSEE, dates, d_start, d_end, window)

res_gr  <- out_gr$forecast_comb

# Calcul MAE/RMSE
metrics_list_INSEE <- list()


for (j in 1:3) {
  col_name <- paste0("Mois_", j)
  df_eval <- data.frame(
    Obs   = y_target[(window + 1): limit_index],
    Text   = mes_modeles_INSEE$Text[(window + 1): limit_index, j],
    noText   = mes_modeles_INSEE$NoText[(window + 1): limit_index, j],
    Rolling   = mes_modeles_INSEE$Rolling[(window + 1): limit_index, j],
    All   = mes_modeles_INSEE$All[(window + 1): limit_index, j],
    Text_FR   = mes_modeles_INSEE$Text_FR[(window + 1): limit_index, j],
    AVG   = res_avg[, j],
    INV   = res_inv[, j],
    GR    = res_gr[, j]
  )
  
  df_eval <- na.omit(df_eval)
  res_horizon <- sapply(df_eval[, -1], function(preds) calc_errors(df_eval$Obs, preds))
  metrics_list_INSEE[[col_name]] <- res_horizon
}

################################################################################
# Tous les modèles LLMs
################################################################################

#Déclaration des modèles
text_INSEE<- read_xlsx("Final_Results/INSEE_text_2020.xlsx")
rolling_INSEE  <- read_xlsx("Final_Results/INSEE_rolling_text_2020.xlsx")
noText_INSEE <- read_xlsx("Final_Results/INSEE_noText_2020.xlsx")
all_INSEE <- read_xlsx("Final_Results/INSEE_all_2020.xlsx")
text_FR_INSEE <- read_xlsx("Final_Results/INSEE_text_FR_2020.xlsx")

text_BDF  <- read_xlsx("Final_Results/BDF_text_2020.xlsx")
rolling_BDF  <- read_xlsx("Final_Results/BDF_rolling_text_2020.xlsx")
noText_BDF  <- read_xlsx("Final_Results/BDF_noText_2020.xlsx")
all_BDF  <- read_xlsx("Final_Results/BDF_all_2020.xlsx")
text_FR_BDF  <- read_xlsx("Final_Results/BDF_text_FR_2020.xlsx")

#Traitemznt des données de chaque modèles
mes_modeles_llm <- list(Text_INSEE = text_INSEE, NoText_INSEE = noText_INSEE, Rolling_INSEE = rolling_INSEE, Text_FR_INSEE = text_FR_INSEE, All_INSEE = all_INSEE,
                    Text_BDF  = text_BDF , NoText_BDF  = noText_BDF , Rolling_BDF  = rolling_BDF , Text_FR_BDF  = text_FR_BDF , All_BDF  = all_BDF )

for (k in 1:length(mes_modeles_llm)){
  
  # Nettoyage des données de chaque modèle
  tmp <- mes_modeles_llm[[k]] |>
    rowwise() |>
    mutate(Median_forecast = median(c_across(starts_with("forecast")), na.rm = TRUE),
           .keep = "none") |>
    ungroup()
  
  k_def <- as.data.frame(
    matrix(
      tmp$Median_forecast, 
      ncol = 3, 
      byrow = TRUE))
  
  # Ecraser anciens modèles par une version propre
  mes_modeles_llm[[k]] <- k_def[1:limit_index, ]
}

# Exécuter les modèles 

## Moyenne Simple
res_avg <- simple_avg_month_v2(mes_modeles_llm)
res_avg<- res_avg[(window + 1): limit_index,]

## Inverse MSE 
out_inv <- rolling_inversed_weight_month_v2(y_target, mes_modeles_llm, dates, d_start, d_end, window)
res_inv <- out_inv$nowcast

## Granger-Ramanathan 
out_gr <- gr_rolling_month_v2(y_target, mes_modeles_llm, dates, d_start, d_end, window)

res_gr  <- out_gr$forecast_comb

# Calcul MAE/RMSE
metrics_list_all_LLMs <- list()


for (j in 1:3) {
  col_name <- paste0("Mois_", j)
  df_eval <- data.frame(
    Obs   = y_target[(window + 1): limit_index],
    AVG   = res_avg[, j],
    INV   = res_inv[, j],
    GR    = res_gr[, j],
    Text_INSEE   = mes_modeles_llm$Text_INSEE[(window + 1): limit_index, j],
    noText_INSEE    = mes_modeles_llm$NoText_INSEE[(window + 1): limit_index, j],
    Rolling_INSEE    = mes_modeles_llm$Rolling_INSEE[(window + 1): limit_index, j],
    All_INSEE    = mes_modeles_llm$All_INSEE[(window + 1): limit_index, j],
    Text_FR_INSEE    = mes_modeles_llm$Text_FR_INSEE[(window + 1): limit_index, j],
    Text_BDF   = mes_modeles_llm$Text_BDF[(window + 1): limit_index, j],
    noText_BDF   = mes_modeles_llm$NoText_BDF[(window + 1): limit_index, j],
    Rolling_BDF   = mes_modeles_llm$Rolling_BDF[(window + 1): limit_index, j],
    All_BDF   = mes_modeles_llm$All_BDF[(window + 1): limit_index, j],
    Text_FR_BDF   = mes_modeles_llm$Text_FR_BDF[(window + 1): limit_index, j]
    
  )
  
  df_eval <- na.omit(df_eval)
  res_horizon <- sapply(df_eval[, -1], function(preds) calc_errors(df_eval$Obs, preds))
  metrics_list_all_LLMs[[col_name]] <- res_horizon
}



################################################################################
# Tous les modèles économétriques
################################################################################

# MODELES


MF3 <- read_xlsx("SYNTHESE_mb.xlsx", sheet = "ECONOMETRICS")

MF3 <- MF3 |>
  mutate(MF_M1 = MF_M1...8,  
         MF_M2 = MF_M2...9, 
         MF_M3 = MF_M3...10,
         .keep = "none") |>
  slice_head(n = limit_index)

#Ajout données ISMA 
df_ISMA_prev <- df_ISMA |>
  select(dates,forecast_M1:forecast_M3)|>
  slice_head(n = limit_index)

ISMA <- df_ISMA_prev |>
  mutate(
    ISMA_M1 = forecast_M1,
    ISMA_M2 = forecast_M2,
    ISMA_M3 = forecast_M3,
    .keep = "none"
)

#AR
df_AR_prev <- df_AR |>
  select(!PIB_PR) |>
  slice_head(n = limit_index)

AR_def <- data.frame(
  AR_M1 = df_AR_prev$forecast_AR,
  AR_M2 = df_AR_prev$forecast_AR,
  AR_M3 = df_AR_prev$forecast_AR
)
  

#Liste des modèles
mes_modeles_eco <- list(
  ISMA = ISMA,
  MF3  = MF3,
  AR   = AR_def
)


# Exécution des combinaisons

## Moyenne Simple
res_avg <- simple_avg_month_v2(mes_modeles_eco)
# Découpage pour l'évaluation
res_avg_aligned <- res_avg[(window + 1): limit_index, ]

## Inverse MSE 
out_inv <- rolling_inversed_weight_month_v2(y_target, mes_modeles_eco, dates, d_start, d_end, window)
res_inv <- out_inv$nowcast

## Granger-Ramanathan 
out_gr <- gr_rolling_month_v2(y_target, mes_modeles_eco, dates, d_start, d_end, window)
res_gr  <- out_gr$forecast_comb


#Calcul MAE/RMSE

metrics_list_eco <- list()

for (j in 1:3) {
  col_name <- paste0("Mois_", j)
  
  df_eval <- data.frame(
    Obs  = y_target[(window + 1): limit_index],
    AVG  = res_avg_aligned[, j],
    INV  = res_inv[, j],
    GR   = res_gr[, j],
    ISMA = mes_modeles_eco$ISMA[(window + 1): limit_index, j],
    MF3  = mes_modeles_eco$MF3[(window + 1): limit_index, j],
    AR   = mes_modeles_eco$AR[(window + 1): limit_index, j]
    
  )
  
  df_eval <- na.omit(df_eval)
  
  res_horizon <- sapply(df_eval[, -1], function(preds) calc_errors(df_eval$Obs, preds))
  metrics_list_eco[[col_name]] <- res_horizon
}




################################################################################
# Tous les modèles 
################################################################################

mes_modeles_all <- c(mes_modeles, mes_modeles_eco)


# Exécuter les combinaisons

## Moyenne Simple
res_avg <- simple_avg_month_v2(mes_modeles_all)
# Découpage pour l'évaluation (taille 23)
res_avg_aligned <- res_avg[(window + 1): limit_index, ]

## Inverse MSE 
# On passe l'historique complet (taille 43)
out_inv <- rolling_inversed_weight_month_v2(y_target, mes_modeles_all, dates, d_start, d_end, window)
res_inv <- out_inv$nowcast

## Granger-Ramanathan 
# On passe l'historique complet (taille 43)
out_gr <- gr_rolling_month_v2(y_target, mes_modeles_all, dates, d_start, d_end, window)
res_gr  <- out_gr$forecast_comb


# 3. CALCUL MAE/RMSE GLOBAL

metrics_list_all <- list()

# Indices d'évaluation 
id_eval <- (window + 1):limit_index

for (j in 1:3) {
  col_name <- paste0("Mois_", j)
  
  df_eval <- data.frame(
    Obs            = y_target[id_eval],
    
    ## Combinaisons
    AVG            = res_avg_aligned[, j],
    INV            = res_inv[, j],
    GR             = res_gr[, j],
    
    ## Modèles Économétriques
    ISMA           = mes_modeles_eco$ISMA[id_eval, j],
    MF3            = mes_modeles_eco$MF3[id_eval, j],
    AR             = mes_modeles_eco$AR[id_eval, j],
    
    ## Modèles LLM
    Text_INSEE     = mes_modeles_llm$Text_INSEE[id_eval, j],
    NoText_INSEE   = mes_modeles_llm$NoText_INSEE[id_eval, j],
    Rolling_INSEE  = mes_modeles_llm$Rolling_INSEE[id_eval, j],
    Text_FR_INSEE  = mes_modeles_llm$Text_FR_INSEE[id_eval, j],
    All_INSEE      = mes_modeles_llm$All_INSEE[id_eval, j],
    Text_BDF       = mes_modeles_llm$Text_BDF[id_eval, j],
    NoText_BDF     = mes_modeles_llm$NoText_BDF[id_eval, j],
    Rolling_BDF    = mes_modeles_llm$Rolling_BDF[id_eval, j],
    Text_FR_BDF    = mes_modeles_llm$Text_FR_BDF[id_eval, j],
    All_BDF        = mes_modeles_llm$All_BDF[id_eval, j]
  )
  
  df_eval <- na.omit(df_eval)
  
  # Calcul vectoriel des erreurs
  res_horizon <- sapply(df_eval[, -1], function(preds) calc_errors(df_eval$Obs, preds))
  metrics_list_all[[col_name]] <- res_horizon
}













################################################################################
# Autre façon pour vérifier les résultats (INSEE BDF ici)
################################################################################

mes_modeles_manual <- list(BDF = BDF_Text_def, INSEE = INSEE_Text_def )

#  Moyenne Simple

comb_avg_manual <- simple_avg_month_v2(mes_modeles_manual)

# Inverse MSE
comb_MSE_manual <- rolling_inversed_weight_month_v2(
  y = y_ref, 
  model_list = mes_modeles_manual, 
  dates = dates, 
  start_covid = d_start, 
  end_covid = d_end, 
  rolling_window = window
)

# Granger-Ramanathan
comb_gr_manual <- gr_rolling_month_v2(
  y = y_ref, 
  model_list = mes_modeles_manual, 
  dates = dates, 
  start_covid = d_start, 
  end_covid = d_end, 
  rolling_window = window
)

indices_comparaison <- (window + 1):limit_index

# Couper les modèles qui sont entiers (avec la taille du training dataset)
pib_vec_trimmed <- y_ref[indices_comparaison]
bdf_trimmed     <- BDF_Text_def[indices_comparaison, ]
INSEE_trimmed   <- INSEE_Text_def[indices_comparaison, ] 
avg_trimmed <- comb_avg_manual[indices_comparaison, ] 

#modèles déjà sans le training dataset
mse_trimmed <- comb_MSE_manual$nowcast
gr_trimmed  <- comb_gr_manual$forecast_comb


# Calcul MAE/RMSE

results_comparison <- data.frame(
  Methode = c("Moyenne Simple", "Inverse MSE", "Granger-Ramanathan", "BDF_Text", "INSEE"),
  
  # --- RMSE PAR MOIS ---
  RMSE_Mois_1 = c(
    sqrt(mean((pib_vec_trimmed - avg_trimmed$Mois_1)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - mse_trimmed$Mois_1)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - gr_trimmed$Mois_1)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - bdf_trimmed$V1)^2, na.rm = TRUE)),     
    sqrt(mean((pib_vec_trimmed - INSEE_trimmed$V1)^2, na.rm = TRUE)) 
  ),
  
  RMSE_Mois_2 = c(
    sqrt(mean((pib_vec_trimmed - avg_trimmed$Mois_2)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - mse_trimmed$Mois_2)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - gr_trimmed$Mois_2)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - bdf_trimmed$V2)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - INSEE_trimmed$V2)^2, na.rm = TRUE))
  ),
  
  RMSE_Mois_3 = c(
    sqrt(mean((pib_vec_trimmed - avg_trimmed$Mois_3)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - mse_trimmed$Mois_3)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - gr_trimmed$Mois_3)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - bdf_trimmed$V3)^2, na.rm = TRUE)),
    sqrt(mean((pib_vec_trimmed - INSEE_trimmed$V3)^2, na.rm = TRUE))
  ),
  
  # --- MAE PAR MOIS ---
  MAE_Mois_1 = c(
    mean(abs(pib_vec_trimmed - avg_trimmed$Mois_1), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - mse_trimmed$Mois_1), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - gr_trimmed$Mois_1), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - bdf_trimmed$V1), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - INSEE_trimmed$V1), na.rm = TRUE)
  ),
  
  MAE_Mois_2 = c(
    mean(abs(pib_vec_trimmed - avg_trimmed$Mois_2), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - mse_trimmed$Mois_2), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - gr_trimmed$Mois_2), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - bdf_trimmed$V2), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - INSEE_trimmed$V2), na.rm = TRUE)
  ),
  
  MAE_Mois_3 = c(
    mean(abs(pib_vec_trimmed - avg_trimmed$Mois_3), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - mse_trimmed$Mois_3), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - gr_trimmed$Mois_3), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - bdf_trimmed$V3), na.rm = TRUE),
    mean(abs(pib_vec_trimmed - INSEE_trimmed$V3), na.rm = TRUE)
  )
)

results_comparison_final <- results_comparison |>
  arrange(RMSE_Mois_3)










#Test sans suppression covid
##############################################################################################
res_inv_std <- rolling_inversed_weight_month_standard(
  y = y_target, 
  model_list = mes_modeles, 
  rolling_window = 20
)

# Granger-Ramanathan Standard
res_gr_std <- gr_rolling_month_standard(
  y = y_target, 
  model_list = mes_modeles, 
  rolling_window = 20
)


calc_mae_rmse <- function(y, nowcast_df) {
  
  n_y <- length(y)
  n_pred <- nrow(nowcast_df)
  y_obs <- tail(y, n_pred)
  
  res <- matrix(NA_real_, nrow = 2, ncol = ncol(nowcast_df))
  rownames(res) <- c("MAE", "RMSE")
  colnames(res) <- colnames(nowcast_df)
  
  for (j in 1:ncol(nowcast_df)) {
    preds <- nowcast_df[, j]
    err <- preds - y_obs
    
    res["MAE", j]  <- mean(abs(err), na.rm = TRUE)
    res["RMSE", j] <- sqrt(mean(err^2, na.rm = TRUE))
  }
  
  return(as.data.frame(res))
}

res <- calc_mae_rmse(pib$PIB_PR[21:63], cbind(res_gr_std$forecast_comb, res_inv_std$nowcast))
