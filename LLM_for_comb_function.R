# SCRIPT : FORECAST COMBINATION

# BUT : proposer plusieurs combinaison de forecasts entre modèles (LLM et/ou économétrique) avec gestion des périodes et suppression du Covid

rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
# source("Modèle_ISMA.R") 
# source('LLM_AR.R')      

################################################################################
# CONFIGURATION GLOBALE
################################################################################

# Paramètres utilisateur
remove_covid <- FALSE               
cutoff_date  <- as.Date("2022-01-01") 
start_date_common <- as.Date("2015-02-01") 

# Alignement du PIB
pib_subset <- pib |> 
  filter(dates >= start_date_common)

# Variables globales
window      <- 20
limit_index <- 43 

y_target  <- pib_subset$PIB_PR[1:limit_index]
dates_vec <- pib_subset$dates[1:limit_index] 

d_start   <- as.Date("2020-01-01")
d_end     <- as.Date("2021-12-31")


################################################################################
#  FONCTION D'EVALUATION (MAE/RMSE par Période)
################################################################################

# automatisation du calcul des erreurs 

run_eval <- function(title, model_list, y_true, dates_eval, 
                           window, limit, remove_covid, cutoff_date) {
  
  # Exécuter les modèles 
  
  ## Moyenne Simple
  res_avg <- simple_avg_month_v2(model_list)
  res_avg_aligned <- res_avg[(window + 1): limit,]
  
  ## Inverse MSE 
  out_inv <- rolling_inversed_weight_month_v2(y_true, model_list, dates_vec, d_start, d_end, window)
  res_inv <- out_inv$nowcast
  
  ## Granger-Ramanathan 
  out_gr <- gr_rolling_month_v2(y_true, model_list, dates_vec, d_start, d_end, window)
  res_gr  <- out_gr$forecast_comb
  
  
  # Calcul MAE/RMSE
  
  metrics_Global <- list(); metrics_P1 <- list(); metrics_P2 <- list()
  
  idx_eval <- (window + 1):limit
  dates_current <- dates_eval[idx_eval] 
  
  for (j in 1:3) {
    col_name <- paste0("Mois_", j)
    
    # Construction DF
    df_temp <- data.frame(
      Obs = as.numeric(y_true[idx_eval]),
      AVG = res_avg_aligned[, j],
      INV = res_inv[, j],
      GR  = res_gr[, j]
    )
    
    # Calcul pour chaque modèle dans la liste
    for(name in names(model_list)) {
      # Gestion selon format
      val <- if(is.data.frame(model_list[[name]])) model_list[[name]][idx_eval, j] else model_list[[name]][idx_eval]
      df_temp[[name]] <- as.numeric(val) 
    }
    
    df_temp <- na.omit(df_temp)
    
    # Périodes et covid
    is_covid <- dates_current >= as.Date("2020-01-01") & dates_current <= as.Date("2021-12-31")
    is_pre   <- dates_current < cutoff_date
    is_post  <- dates_current >= cutoff_date
    
    # Filtres selon choix 
    if(remove_covid) { 
      mask_global <- !is_covid
      mask_p2 <- is_post & !is_covid 
    } else { 
      mask_global <- TRUE
      mask_p2 <- is_post 
    }
    
    # Calculs via 
    if(sum(mask_global) > 0) {
      metrics_Global[[col_name]] <- sapply(df_temp[mask_global, -1], function(p) calc_errors(df_temp$Obs[mask_global], p))
    }
    
    if(sum(is_pre) > 0) {
      metrics_P1[[col_name]] <- sapply(df_temp[is_pre, -1], function(p) calc_errors(df_temp$Obs[is_pre], p))
    }
    
    if(sum(mask_p2) > 0) {
      metrics_P2[[col_name]] <- sapply(df_temp[mask_p2, -1], function(p) calc_errors(df_temp$Obs[mask_p2], p))
    }
  }
  
  return(list(Global=metrics_Global, P1=metrics_P1, P2=metrics_P2))
}


################################################################################
# INITIALISATION ET NETTOYAGE DES FICHIERS
################################################################################

# Fonction  pour nettoyer les données organisées de la même manière
clean_llm_excel <- function(filename, limit) {
  df <- read_xlsx(filename) |> 
    rowwise() |>
    mutate(Median_forecast = median(c_across(starts_with("forecast")), na.rm = TRUE), .keep = "none") |> ungroup()
  mat <- matrix(df$Median_forecast, ncol = 3, byrow = TRUE)
  df_clean <- as.data.frame(mat)
  colnames(df_clean) <- c("Mois_1", "Mois_2", "Mois_3")
  return(df_clean[1:limit, ])
}

#BDF
text_BDF      <- clean_llm_excel("Final_Results/BDF_text_2020.xlsx", limit_index)
rolling_BDF   <- clean_llm_excel("Final_Results/BDF_rolling_text_2020.xlsx", limit_index)
noText_BDF    <- clean_llm_excel("Final_Results/BDF_noText_2020.xlsx", limit_index)
all_BDF       <- clean_llm_excel("Final_Results/BDF_all_2020.xlsx", limit_index)
text_FR_BDF   <- clean_llm_excel("Final_Results/BDF_text_FR_2020.xlsx", limit_index)

mes_modeles_BDF_ALL <- list(Text=text_BDF, NoText=noText_BDF, Rolling=rolling_BDF, FR=text_FR_BDF, All=all_BDF)

#INSEE
text_INSEE    <- clean_llm_excel("Final_Results/INSEE_text_2020.xlsx", limit_index)
rolling_INSEE <- clean_llm_excel("Final_Results/INSEE_rolling_text_2020.xlsx", limit_index)
noText_INSEE  <- clean_llm_excel("Final_Results/INSEE_noText_2020.xlsx", limit_index)
all_INSEE     <- clean_llm_excel("Final_Results/INSEE_all_2020.xlsx", limit_index)
text_FR_INSEE <- clean_llm_excel("Final_Results/INSEE_text_FR_2020.xlsx", limit_index)

mes_modeles_INSEE_ALL <- list(Text=text_INSEE, NoText=noText_INSEE, Rolling=rolling_INSEE, FR=text_FR_INSEE, All=all_INSEE)

#ECONOMETRIE

# MF3
MF3 <- read_xlsx("SYNTHESE_mb.xlsx", sheet = "ECONOMETRICS") |>
  mutate(Mois_1=`MF_M1...8`, Mois_2=`MF_M2...9`, Mois_3=`MF_M3...10`, .keep="none") |> 
  slice_head(n=limit_index) |>
  mutate(across(everything(), as.numeric)) |> 
  as.data.frame() 

# ISMA
if(!exists("df_ISMA")) df_ISMA <- read_xlsx("Results_ISMA.xlsx") 
ISMA <- df_ISMA |> 
  filter(dates >= start_date_common) |> 
  select(forecast_M1:forecast_M3) |> 
  slice_head(n=limit_index)
colnames(ISMA) <- c("Mois_1", "Mois_2", "Mois_3")
ISMA <- ISMA |> mutate(across(everything(), as.numeric)) |> as.data.frame()

# AR
if(!exists("df_AR")) df_AR <- read_xlsx("Results_AR.xlsx")
df_AR_al <- df_AR |> 
  filter(dates >= start_date_common) |> 
  slice_head(n=limit_index)
AR_def <- data.frame(
  Mois_1=as.numeric(df_AR_al$forecast_AR), 
  Mois_2=as.numeric(df_AR_al$forecast_AR), 
  Mois_3=as.numeric(df_AR_al$forecast_AR)
)

mes_modeles_ECO_ALL <- list(ISMA=ISMA, MF3=MF3, AR=AR_def)


################################################################################
# EXECUTION DES COMBINAISONS
################################################################################

#  1 : INSEE Text + BDF Text
mes_modeles <- list(INSEE = text_INSEE, BDF = text_BDF)
res_BDF_INSEE_Text <- run_eval("COMBINAISON : BDF Text + INSEE Text", mes_modeles, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date)

# 2 : BDF Text + ISMA 
mes_modeles <- list(BDF = text_BDF, ISMA = ISMA)
res_BDF_Text_ISMA <- run_eval("COMBINAISON : BDF Text + ISMA", mes_modeles, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date)

#  3 : TOUS LES MODELES BDF 
res_BDF <- run_eval("COMBINAISON : TOUS MODELES BDF", mes_modeles_BDF_ALL, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date)

# 4 : TOUS LES MODELES INSEE 
res_INSEE <- run_eval("COMBINAISON : TOUS MODELES INSEE", mes_modeles_INSEE_ALL, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date)

#  5 : TOUS LES MODELES ECONOMETRIQUES
res_ECO <- run_eval("COMBINAISON : TOUS MODELES ECO", mes_modeles_ECO_ALL, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date)

# TOUS LES MODELES (GLOBAL) 
names(mes_modeles_BDF_ALL) <- paste0("BDF_", names(mes_modeles_BDF_ALL))
names(mes_modeles_INSEE_ALL) <- paste0("INSEE_", names(mes_modeles_INSEE_ALL))
mes_modeles_all <- c(mes_modeles_BDF_ALL, mes_modeles_INSEE_ALL, mes_modeles_ECO_ALL)

res_ALL <- run_eval("COMBINAISON : TOUS LES MODELES (LLM + ECO)", mes_modeles_all, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date)