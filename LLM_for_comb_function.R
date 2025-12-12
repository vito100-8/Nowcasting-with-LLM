# SCRIPT : FORECAST COMBINATION

# BUT : proposer plusieurs combinaison de forecasts entre modèles (LLM et/ou économétrique) avec gestion des périodes et suppression du Covid


source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("LLM_AR_climat.R")
     

################################################################################
# CONFIGURATION GLOBALE
################################################################################


#OUVRIR FICHIER COVID
pib <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")

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
                     window, limit, remove_covid, cutoff_date, 
                     use_conf = TRUE) {
  

  # On sépare les prévisions (cols 1-3) des niveaux confiances (cols 4-6)
  
  model_list_clean <- list() # Pour les prev
  list_confidence  <- list() # Pour les  niveaux de conf
  
  for(name in names(model_list)) {
    data <- model_list[[name]]
    
    if(is.data.frame(data)) {
      model_list_clean[[name]] <- data[, 1:3]
      
      # Si on a 6 colonnes, on extrait la confiance
      if(ncol(data) >= 6) {
        list_confidence[[name]] <- data[, 4:6]
      }
    } else {
      # Cas où c'est une matrice
      model_list_clean[[name]] <- data
    }
  }
  
  
  ## Moyenne Simple (sur prévisions uniquement)
  res_avg <- simple_avg_month(model_list_clean)
  res_avg_aligned <- res_avg[(window + 1): limit,]
  
  ## Inverse MSE 
  out_inv <- rolling_inversed_weight_month(y_true, model_list_clean, dates_vec, d_start, d_end, window)
  res_inv <- out_inv$nowcast
  
  ## Granger-Ramanathan 
  out_gr <- gr_rolling_month(y_true, model_list_clean, dates_vec, d_start, d_end, window)
  res_gr  <- out_gr$forecast_comb
  
  ## Niveau de confiance
  res_conf <- NULL
  # Lancement ssi modèle LLM
  if(use_conf && length(list_confidence) > 0) {
    out_conf <- rolling_confidence_weight(model_list_clean, list_confidence)
    res_conf <- out_conf[(window + 1): limit,]
  }
  
 #Calcul MAE/RMSE

  idx_eval <- (window + 1):limit
  dates_current <- dates_eval[idx_eval] 
  
  metrics_Global <- list(); metrics_P1 <- list(); metrics_P2 <- list()
  
  for (j in 1:3) {
    col_name <- paste0("Mois_", j)
    
    df_temp <- data.frame(
      Obs = as.numeric(y_true[idx_eval]),
      AVG = res_avg_aligned[, j],
      INV = res_inv[, j],
      GR  = res_gr[, j]
    )
    
    if(use_conf && !is.null(res_conf)){
      df_temp$CONF <- res_conf[,j]
    }
    
    for(name in names(model_list_clean)) {
      val <- if(is.data.frame(model_list_clean[[name]])) model_list_clean[[name]][idx_eval, j] else model_list_clean[[name]][idx_eval]
      df_temp[[name]] <- as.numeric(val) 
    }
    
    df_temp <- na.omit(df_temp)
    is_covid <- dates_current >= as.Date("2020-01-01") & dates_current <= as.Date("2021-12-31")
    is_pre   <- dates_current < cutoff_date
    is_post  <- dates_current >= cutoff_date
    
    if(remove_covid) { mask_global <- !is_covid; mask_p2 <- is_post & !is_covid } else { mask_global <- TRUE; mask_p2 <- is_post }
    
    if(sum(mask_global) > 0) metrics_Global[[col_name]] <- sapply(df_temp[mask_global, -1], function(p) calc_errors(df_temp$Obs[mask_global], p))
    if(sum(is_pre) > 0)      metrics_P1[[col_name]]     <- sapply(df_temp[is_pre, -1], function(p) calc_errors(df_temp$Obs[is_pre], p))
    if(sum(mask_p2) > 0)     metrics_P2[[col_name]]     <- sapply(df_temp[mask_p2, -1], function(p) calc_errors(df_temp$Obs[mask_p2], p))
  }
  
  return(list(Global=metrics_Global, P1=metrics_P1, P2=metrics_P2))
}

################################################################################
# INITIALISATION ET NETTOYAGE DES FICHIERS
################################################################################

# Fonction  pour nettoyer les données organisées de la même manière
## Renvoie un df de forecast mais aussi des niveaux de confiance
clean_llm_excel <- function(filename, limit) {

  df <- read_xlsx(filename) |> 
    rowwise() |>
    mutate(
      Median_forecast = median(c_across(starts_with("forecast")), na.rm = TRUE), 
      Median_conf  = median(c_across(starts_with("confidence")), na.rm = TRUE)
    ) |> 
    ungroup()
  
  #  Forecasts (Colonnes 1 à 3)
  mat_for <- matrix(df$Median_forecast, ncol = 3, byrow = TRUE)
  df_for <- as.data.frame(mat_for)
  colnames(df_for) <- c("Mois_1", "Mois_2", "Mois_3")
  
  # Confiance (Colonnes 4 à 6)
  mat_conf <- matrix(df$Median_conf, ncol = 3, byrow = TRUE)
  df_conf <- as.data.frame(mat_conf)
  colnames(df_conf) <- c("Conf_1", "Conf_2", "Conf_3")
  
  # Fusion en un df
  df_final <- cbind(df_for, df_conf)
  
  return(df_final[1:limit, ])
}
#BDF
BDF_txt    <- clean_llm_excel("Final_Results/BDF_text_2020.xlsx", limit_index)
BDF_txtrol  <- clean_llm_excel("Final_Results/BDF_rolling_text_2020.xlsx", limit_index)
BDF_txtTS      <- clean_llm_excel("Final_Results/BDF_all_2020.xlsx", limit_index)
BDF_txtFR  <- clean_llm_excel("Final_Results/BDF_text_FR_2020.xlsx", limit_index)

mes_modeles_BDF_ALL <- list(BDF_txt = BDF_txt, BDF_txtrol=BDF_txtrol, BDF_txtFR=BDF_txtFR, BDF_txtTS=BDF_txtTS)

#INSEE
INSEE_txt    <- clean_llm_excel("Final_Results/INSEE_text_2020.xlsx", limit_index)
INSEE_txtrol <- clean_llm_excel("Final_Results/INSEE_rolling_text_2020.xlsx", limit_index)
INSEE_txtTS    <- clean_llm_excel("Final_Results/INSEE_all_2020.xlsx", limit_index)
INSEE_txtFR <- clean_llm_excel("Final_Results/INSEE_text_FR_2020.xlsx", limit_index)

mes_modeles_INSEE_ALL <- list(INSEE_txt = INSEE_txt, INSEE_txtrol=INSEE_txtrol, INSEE_txtFR=INSEE_txtFR, INSEE_txtTS=INSEE_txtTS)

#ECONOMETRIE - Modèle Climat 

modèle_clim_BDF_IND <- df_AR_climat_BDF_IND |> 
    select(BDF_IND_M1:BDF_IND_M3) |>
   slice_tail(n=limit_index) |> 
   as.data.frame()

modèle_clim_INSEE_IND <- df_AR_climat_INSEE_IND |> 
  select(INSEE_IND_M1:INSEE_IND_M3) |>
  slice_tail(n=limit_index) |> 
  as.data.frame()

modèle_clim_BDF_ALL <- df_AR_climat_BDF_ALL |> 
  select(BDF_ALL_M1:BDF_ALL_M3) |>
  slice_tail(n=limit_index) |> 
  as.data.frame()

modèle_clim_INSEE_ALL <- df_AR_climat_INSEE_ALL |> 
  select(INSEE_ALL_M1:INSEE_ALL_M3) |>
  slice_tail(n=limit_index) |> 
  as.data.frame()

modèle_clim_COMB_IND <- df_AR_climat_COMB_IND|> 
  select(COMB_IND_M1:COMB_IND_M3) |>
  slice_tail(n=limit_index) |> 
  as.data.frame()

modèle_clim_COMB_ALL <- df_AR_climat_COMB_ALL |> 
  select(COMB_ALL_M1:COMB_ALL_M3) |>
  slice_tail(n=limit_index) |> 
  as.data.frame()

################################################################################
# EXECUTION DES COMBINAISONS
################################################################################

#  1 : INSEE Text + BDF Text 
mes_modeles <- list(BDF_txt=BDF_txt, INSEE_txt = INSEE_txt)
res_BDF_INSEE_Text <- run_eval("COMBINAISON : BDF Text + INSEE Text", mes_modeles, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date, 
                               use_conf = TRUE)

# 2 : BDF Text + Climat BDF 
mes_modeles <- list(BDF_txt=BDF_txt, Clim = modèle_clim_BDF_ALL)
res_BDF_Text_Clim <- run_eval("COMBINAISON : BDF Text + Climat Industrie BDF", mes_modeles, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date, 
                              use_conf = FALSE)

# 3  : TOUS LES MODELES BDF
res_BDF <- run_eval("COMBINAISON : TOUS MODELES BDF", mes_modeles_BDF_ALL, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date, 
                    use_conf = TRUE)

# 4 : TOUS LES MODELES INSEE 
res_INSEE <- run_eval("COMBINAISON : TOUS MODELES INSEE", mes_modeles_INSEE_ALL, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date, 
                      use_conf = TRUE)

# 5 TOUS LES MODELES LLM 
mes_modeles_LLM <-  c(mes_modeles_BDF_ALL, mes_modeles_INSEE_ALL)
res_LLM <- run_eval("COMBINAISON : TOUS MODELES LLM", mes_modeles_LLM, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date, 
                    use_conf = TRUE)

# 6 : TOUS LES MODELES ECONOMETRIQUES 
mes_modeles_ECO <- list(BDF_IND = modèle_clim_BDF_IND, BDF_ALL = modèle_clim_BDF_ALL,
                        INSEE_ALL= modèle_clim_INSEE_ALL, INSEE_IND = modèle_clim_INSEE_IND,
                        COMB_ALL = modèle_clim_COMB_ALL, COMB_IND = modèle_clim_COMB_IND)
res_ECO <- run_eval("COMBINAISON : TOUS MODELES ECO", mes_modeles_ECO,  y_target, dates_vec, window, limit_index, remove_covid, cutoff_date, 
                    use_conf = FALSE)

# 7 :  TOUS LES MODELES (GLOBAL) 
mes_modeles_all <- c(mes_modeles_LLM, mes_modeles_ECO)
res_ALL <- run_eval("COMBINAISON : TOUS LES MODELES (LLM + ECO)", mes_modeles_all, y_target, dates_vec, window, limit_index, remove_covid, cutoff_date, 
                    use_conf = FALSE)