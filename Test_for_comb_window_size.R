#Comparaison window = 10 vs 20 dans les forecasts combinations

source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("LLM_AR_climat.R")


################################################################################
# CONFIGURATION GLOBALE
################################################################################


#OUVRIR FICHIER PIB
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
  
  #On évalue toujours de 2020 à 2025 
  start_eval <- 21
  idx_pib <- start_eval:limit 
  dates_current <- dates_eval[idx_pib]
  
  # Préparation des list et configuration
  model_list_clean <- list(); list_confidence  <- list()
  for(name in names(model_list)) {
    data <- model_list[[name]]
    if(is.data.frame(data)) {
      model_list_clean[[name]] <- data[, 1:3]
      if(ncol(data) >= 6) list_confidence[[name]] <- data[, 4:6]
    } else { model_list_clean[[name]] <- data }
  }
  
  # Calcul combinaisons 
  res_avg <- simple_avg_month(model_list_clean)
  res_avg_aligned <- res_avg[idx_pib, ]
  
  out_inv <- rolling_inversed_weight_month(y_true, model_list_clean, dates_vec, d_start, d_end, window)
  out_gr  <- gr_rolling_month(y_true, model_list_clean, dates_vec, d_start, d_end, window)
  
  # shift des fonctions pour que les résultats soient tous sur la même fenêtre
  shift <- start_eval - (window + 1)
  
  res_inv_fixed <- out_inv$nowcast[(shift + 1):(shift + length(idx_pib)), ]
  res_gr_fixed  <- out_gr$forecast_comb[(shift + 1):(shift + length(idx_pib)), ]
  
  res_conf_fixed <- NULL
  if(use_conf && length(list_confidence) > 0) {
    out_conf <- rolling_confidence_weight(model_list_clean, list_confidence)
    res_conf_fixed <- out_conf[(shift + 1):(shift + length(idx_pib)), ]
  }
  
  #  CALCUL MAE/RMSE
  metrics_Global <- list(); metrics_P1 <- list(); metrics_P2 <- list()
  
  for (j in 1:3) {
    col_name <- paste0("Mois_", j)
    
    #création df
    df_temp <- data.frame(
      Obs = as.numeric(y_true[idx_pib]),
      AVG = res_avg_aligned[, j],
      INV = res_inv_fixed[, j],
      GR  = res_gr_fixed[, j]
    )
    
    if(!is.null(res_conf_fixed)) df_temp$CONF <- res_conf_fixed[, j]
    
    # Alignement des modèles individuels
    for(name in names(model_list_clean)) {
      val <- if(is.data.frame(model_list_clean[[name]])) model_list_clean[[name]][idx_pib, j] else model_list_clean[[name]][idx_pib]
      df_temp[[name]] <- as.numeric(val)
    }
    

    #Création des périodes globales et pré/post covid
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


#on transforme en fonction qui sera exécutée pour les deux tailles de fenêtres
run_all_combinations <- function(win_size) {
  
 
  # 1 : INSEE Text + BDF Text 
  res_BDF_INSEE_Text <- run_eval("BDF+INSEE", list(BDF_txt=BDF_txt, INSEE_txt=INSEE_txt), y_target, dates_vec, win_size, limit_index, remove_covid, cutoff_date, TRUE)
  
  # 2 : BDF Text + Climat BDF 
  res_BDF_Text_Clim <- run_eval("BDF+Climat", list(BDF_txt=BDF_txt, Clim=modèle_clim_BDF_ALL), y_target, dates_vec, win_size, limit_index, remove_covid, cutoff_date, FALSE)
  
  # 3 : TOUS BDF
  res_BDF <- run_eval("All BDF", mes_modeles_BDF_ALL, y_target, dates_vec, win_size, limit_index, remove_covid, cutoff_date, TRUE)
  
  # 4 : TOUS INSEE
  res_INSEE <- run_eval("All INSEE", mes_modeles_INSEE_ALL, y_target, dates_vec, win_size, limit_index, remove_covid, cutoff_date, TRUE)
  
  # 5 : TOUS LLM
  mes_modeles_LLM <- c(mes_modeles_BDF_ALL, mes_modeles_INSEE_ALL)
  res_LLM <- run_eval("All LLM", mes_modeles_LLM, y_target, dates_vec, win_size, limit_index, remove_covid, cutoff_date, TRUE)
  
  # 6 : TOUS ECO
  mes_modeles_ECO <- list(BDF_IND = modèle_clim_BDF_IND, BDF_ALL = modèle_clim_BDF_ALL, INSEE_ALL= modèle_clim_INSEE_ALL, INSEE_IND = modèle_clim_INSEE_IND, COMB_ALL = modèle_clim_COMB_ALL, COMB_IND = modèle_clim_COMB_IND)
  res_ECO <- run_eval("All ECO", mes_modeles_ECO, y_target, dates_vec, win_size, limit_index, remove_covid, cutoff_date, FALSE)
  
  # 7 : TOUS GLOBAL
  mes_modeles_all <- c(mes_modeles_LLM, mes_modeles_ECO)
  res_ALL <- run_eval("All Models", mes_modeles_all, y_target, dates_vec, win_size, limit_index, remove_covid, cutoff_date, FALSE)
  
  # Retourner une liste nommée
  return(list(
    "BDF+INSEE" = res_BDF_INSEE_Text,
    "BDF+Climat" = res_BDF_Text_Clim,
    "All BDF" = res_BDF,
    "All INSEE" = res_INSEE,
    "All LLM" = res_LLM,
    "All ECO" = res_ECO,
    "All Models" = res_ALL
  ))
}







################################################################################
# EXÉCUTION DES DEUX SCÉNARIOS
################################################################################

# Scénario A : Window = 20 
results_w20 <- run_all_combinations(win_size = 20)

# Scénario B : Window = 10 
results_w10 <- run_all_combinations(win_size = 10)


################################################################################
#  COMPARAISON (RMSE)
################################################################################

# Fonction pour extraire le RMSE 
extract_rmse_summary <- function(res_list, window_label) {
  df_summary <- data.frame()
  
  for(scenario in names(res_list)) {
    methods <- c("AVG", "INV", "GR")
    
    for(met in methods) {
      val <- res_list[[scenario]]$Global$Mois_1["RMSE", met] ## changer ici le mois voulu
      if(!is.null(val)) {
        df_summary <- rbind(df_summary, data.frame(
          Scenario = scenario,
          Method = met,
          Window = window_label,
          RMSE = as.numeric(val)
        ))
      }
    }
  }
  return(df_summary)
}

df_w20 <- extract_rmse_summary(results_w20, "Window = 20")
df_w10 <- extract_rmse_summary(results_w10, "Window = 10")

df_compare <- bind_rows(df_w20, df_w10)

################################################################################
# 3. VISUALISATION
################################################################################

# Graphique de comparaison
ggplot(df_compare, aes(x = Scenario, y = RMSE, fill = Window)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~Method, ncol = 1) +
  
  # Esthétique
  scale_fill_manual(values = c("Window = 10" = "#3498DB", "Window = 20" = "#2C3E50")) +
  labs(
    title = "First month RMSE comparison between window size",
    y = "RMSE",
    x = ""
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tableau comparatif
df_diff <- df_compare |>
  pivot_wider(names_from = Window, values_from = RMSE) |>
  mutate(Difference = `Window = 10` - `Window = 20`) |>
  arrange(desc(abs(Difference)))

