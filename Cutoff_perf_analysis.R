# Statistiques Descriptives Resultats

source("Library_Nowcasting_LLM.R")


#########################
#Cutoff date et COVID
##########################

cutoff <- as.Date("2025-01-01") # à modifier si évolution

COVID <- 1 # 0 = on ne prend pas la période covid;  1 = si

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

####################################
#Fonction analyse cutoff
###########################
analyze_cutoff_accuracy <- function(file_path, model_name, pib_data, cutoff_date, dummy) {
  

  df_model <- read_xlsx(file_path) 
  
  if (dummy == 0){
    df_model <- df_model |>
      filter(!(Date > "2020-01-01" & Date <= "2021-01-12"))
  }
  
  # B. Nettoyage 
  df_prep <- df_model |>
    mutate(Date = as.Date(Date)) |> 
    rowwise() |>
    mutate(median_f = median(c_across(starts_with("forecast_")), na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      month = month(Date), 
      year = year(Date),
      forecast_quarter = case_when(
        month %in% c(2, 3, 4) ~ 1, month %in% c(5, 6, 7) ~ 2,
        month %in% c(8, 9, 10) ~ 3, month %in% c(11, 12, 1) ~ 4
      ),
      forecast_year = case_when(month == 1 ~ year - 1, TRUE ~ year)
    )
  
  # Join avec le PIB
  df_joined <- left_join(df_prep, pib_data, by = c("forecast_year", "forecast_quarter"))
  
  # Erreur absolue
  df_errors <- df_joined |>
    filter(!is.na(PIB_PR)) |>
    mutate(
      Error = PIB_PR - median_f,
      Abs_Error = abs(Error),
      Period = ifelse(Date.x < cutoff_date, "Pre-Cutoff", "Post-Cutoff")
    )
  
 
  #Stats
  n_pre <- sum(df_errors$Period == "Pre-Cutoff", na.rm = TRUE)
  n_post <- sum(df_errors$Period == "Post-Cutoff", na.rm = TRUE)
  
  
  
  # Initialisation des variables 
  p_val <- NA
  
  
  # Test (si assez de données)
  if(n_pre > 1 & n_post > 1) {
    try({
      test_res <- t.test(Abs_Error ~ Period, data = df_errors, var.equal = FALSE)
      p_val <- test_res$p.value
    }, silent = TRUE)
  }
  
  #Stats
  summary_stats <- df_errors |>
    group_by(Period) |>
    summarise(
      MAE = mean(Abs_Error, na.rm = TRUE),
      RMSE = sqrt(mean(Error^2, na.rm = TRUE)),
      N_Obs = n(),
      .groups = "drop"
    ) |>
    pivot_wider(names_from = Period, values_from = c(MAE, RMSE, N_Obs)) 
  
  # Si période manquante
  if(!"MAE_Post-Cutoff" %in% names(summary_stats)) summary_stats$`MAE_Post-Cutoff` <- NA
  if(!"MAE_Pre-Cutoff" %in% names(summary_stats)) summary_stats$`MAE_Pre-Cutoff` <- NA
  
  summary_stats <- summary_stats |>
    mutate(
      Model = model_name,
      P_Value_T_Test = round(p_val, 4),
      Diff_MAE = `MAE_Post-Cutoff` - `MAE_Pre-Cutoff`,
      Significatif = ifelse(!is.na(P_Value_T_Test) & P_Value_T_Test < 0.05, "OUI", "NON")
    ) |>
    select(Model, any_of(c("MAE_Pre-Cutoff", "MAE_Post-Cutoff", "Diff_MAE", "Significatif", "P_Value_T_Test")), everything())
  
  return(summary_stats)
}
############
#Exécution sur tous les modèles
############

# Liste des modèles
files_list <- list(
  # --- Modèles BDF ---
  
  "BDF_txt" ="Final_results/BDF_text_2020.xlsx",
  "BDF_txtrol" ="Final_results/BDF_rolling_text_2020.xlsx", 
  "BDF_txtO" ="Final_results/BDF_just_text_2020.xlsx", 
  "BDF_txtTS" ="Final_results/BDF_all_2020.xlsx", 
  "BDF_TS" ="Final_results/BDF_excel_2020.xlsx", 
  "BDF_txtFR" ="Final_results/BDF_text_FR_2020.xlsx", 
  
  
  # --- Modèles INSEE ---
  
  "INSEE_txt" ="Final_results/INSEE_text_2020.xlsx",
  "INSEE_notxt" ="Final_results/INSEE_noText_2020.xlsx", 
  "INSEE_txtrol" ="Final_results/INSEE_rolling_text_2020.xlsx", 
  "INSEE_txtO" ="Final_results/INSEE_just_text_2020.xlsx", 
  "INSEE_txtTS" ="Final_results/INSEE_all_2020.xlsx", 
  "INSEE_TS" ="Final_results/INSEE_excel_2020.xlsx", 
  "INSEE_txtFR" ="Final_results/INSEE_text_FR_2020.xlsx", 
  
  
  # --- Modèles ECO ---
  
  "all_txt" ="Final_results/ECO_text_2020.xlsx"
  
)

# Boucle pour tout calculer
results_list <- list()

for(model in names(files_list)) {
  print(paste("Traitement de :", model))
  path <- files_list[[model]]
  
  try({
    results_list[[model]] <- analyze_cutoff_accuracy(path, model, pib, cutoff, COVID)
  })
}

# Tableau final
final_cutoff_analysis <- bind_rows(results_list)

write.xlsx(final_cutoff_analysis, "Analysis_cutoff.xlsx")



######################################################################
# Split periods
####################################################################

cutoff_1 <- as.Date("2020-02-01")

####################################
#Fonction analyse période
###########################
analyze_period_accuracy <- function(file_path, model_name, pib_data, cutoff_date, dummy) {
  
  
  df_model <- read_xlsx(file_path)
  
  if (dummy == 0){
    df_model <- df_model |>
      filter(!(Date >= "2020-02-01" & Date <= "2021-02-01"))
  }
  
  # B. Nettoyage 
  df_prep <- df_model |>
    mutate(Date = as.Date(Date)) |> 
    rowwise() |>
    mutate(median_f = median(c_across(starts_with("forecast_")), na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      month = month(Date), 
      year = year(Date),
      forecast_quarter = case_when(
        month %in% c(2, 3, 4) ~ 1, month %in% c(5, 6, 7) ~ 2,
        month %in% c(8, 9, 10) ~ 3, month %in% c(11, 12, 1) ~ 4
      ),
      forecast_year = case_when(month == 1 ~ year - 1, TRUE ~ year)
    )
  
  # Join avec le PIB
  df_joined <- left_join(df_prep, pib_data, by = c("forecast_year", "forecast_quarter"))
  
  # Erreur absolue
  df_errors <- df_joined |>
    filter(!is.na(PIB_PR)) |>
    mutate(
      Errors = PIB_PR - median_f,
      Abs_Error = abs(Errors),
      Period = ifelse(Date.x < cutoff_1, "Period1", "Period2")
    )
  
  
  #Stats
  n_pre <- sum(df_errors$Period == "Period1", na.rm = TRUE)
  n_post <- sum(df_errors$Period == "Period2", na.rm = TRUE)
  
  
  
  # Initialisation des variables 
  p_val <- NA
  
  
  # Test (si assez de données)
  if(n_pre > 1 & n_post > 1) {
    try({
      test_res <- t.test(Abs_Error ~ Period, data = df_errors, var.equal = FALSE)
      p_val <- test_res$p.value
    }, silent = TRUE)
  }
  
  #Stats
  summary_stats <- df_errors |>
    group_by(Period) |>
    summarise(
      MAE = mean(Abs_Error, na.rm = TRUE),
      RMSE = sqrt(mean(Errors^2, na.rm = TRUE)),
      N_Obs = n(),
      .groups = "drop"
    ) |>
    pivot_wider(names_from = Period, values_from = c(MAE, RMSE, N_Obs)) 
  
  # Si période manquante
  if(!"MAE_Period1" %in% names(summary_stats)) summary_stats$`MAE_Period1` <- NA
  if(!"MAE_Period2" %in% names(summary_stats)) summary_stats$`MAE_Period2` <- NA
  
  summary_stats <- summary_stats |>
    mutate(
      Model = model_name,
      P_Value_T_Test = round(p_val, 4),
      Diff_MAE = `MAE_Period2` - `MAE_Period1`,
      Significatif = ifelse(!is.na(P_Value_T_Test) & P_Value_T_Test < 0.05, "OUI", "NON")
    ) |>
    select(Model, any_of(c("MAE_Period1", "MAE_Period2", "Diff_MAE", "Significatif", "P_Value_T_Test")), everything())
  
  return(summary_stats)
}
############
#Exécution sur tous les modèles
############

# Liste des fichiers
files_list <- list(
  # --- Modèles BDF ---

  
  "BDF_txt" ="Final_results/BDF_text_2020.xlsx",
  "BDF_txtrol" ="Final_results/BDF_rolling_text_2020.xlsx", 
  "BDF_txtO" ="Final_results/BDF_just_text_2020.xlsx", 
  "BDF_txtTS" ="Final_results/BDF_all_2020.xlsx", 
  "BDF_TS" ="Final_results/BDF_excel_2020.xlsx", 
  "BDF_txtFR" ="Final_results/BDF_text_FR_2020.xlsx", 
  
  
  # --- Modèles INSEE ---
  
  "INSEE_txt" ="Final_results/INSEE_text_2020.xlsx",
  "INSEE_notxt" ="Final_results/INSEE_noText_2020.xlsx", 
  "INSEE_txtrol" ="Final_results/INSEE_rolling_text_2020.xlsx", 
  "INSEE_txtO" ="Final_results/INSEE_just_text_2020.xlsx", 
  "INSEE_txtTS" ="Final_results/INSEE_all_2020.xlsx", 
  "INSEE_TS" ="Final_results/INSEE_excel_2020.xlsx", 
  "INSEE_txtFR" ="Final_results/INSEE_text_FR_2020.xlsx", 
  
  
  # --- Modèles ECO ---
  
  "all_txt" ="Final_results/ECO_text_2020.xlsx"
)

# Boucle pour tout calculer
results_list <- list()

for(model in names(files_list)) {
  print(paste("Traitement de :", model))
  path <- files_list[[model]]
  
  try({
    results_list[[model]] <- analyze_period_accuracy(path, model, pib, cutoff_1, COVID)
  })
}

# Tableau final
final_period_analysis <- bind_rows(results_list)

write.xlsx(final_period_analysis, "Analysis_Period.xlsx")

#############################
#Period analysis monthly
############################

analyze_period_monthly_accuracy <- function(file_path, model_name, pib_data, dummy_covid) {
  
  
  df_model <- read_xlsx(file_path) 
  
  # Filtre COVID
  if (dummy_covid == 0){
    df_model <- df_model |>
      filter(!(Date >= as.Date("2020-02-01") & Date < as.Date("2022-02-01")))
  }
  
  #Nettoyage
  df_prep <- df_model |>
    mutate(Date = as.Date(Date)) |> 
    rowwise() |>
    mutate(median_f = median(c_across(starts_with("forecast_")), na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      month = month(Date), 
      year = year(Date),
      forecast_quarter = case_when(
        month %in% c(2, 3, 4) ~ 1, month %in% c(5, 6, 7) ~ 2,
        month %in% c(8, 9, 10) ~ 3, month %in% c(11, 12, 1) ~ 4
      ),
      # Tri par mois dans le trimestre
      month_in_quarter = case_when( 
        month %in% c(2, 5, 8, 11) ~ 1, month %in% c(3, 6, 9, 12) ~ 2,
        month %in% c(4, 7, 10, 1) ~ 3
      ),
      forecast_year = case_when(month == 1 ~ year - 1, TRUE ~ year)
    )
  
  # Join
  df_joined <- left_join(df_prep, pib_data, by = c("forecast_year", "forecast_quarter"))
  
  # Calcul Err abs sur la période
  df_errors <- df_joined |>
    filter(!is.na(PIB_PR)) |>
    mutate(
      Abs_Error = abs(PIB_PR - median_f),
      Error_Squared = (PIB_PR - median_f)^2,
      Period_Group = case_when(
        forecast_year < 2020 ~ "2015_2019",
        forecast_year >=  2020 ~ "2020_2025"

      ))
  
  # Agrégation par mois et par période
  summary_stats <- df_errors |>
    group_by(Period_Group, month_in_quarter) |> 
    summarise(
      MAE = mean(Abs_Error, na.rm = TRUE),
      RMSE = sqrt(mean(Error_Squared, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # F. PIVOTAGE POUR CRÉER UNE LIGNE PAR MODÈLE
  final_wide <- summary_stats |>
    # Pour avoir une colonne par mois dans le trimestre
    pivot_wider(
      names_from = c(Period_Group, month_in_quarter), 
      values_from = c(MAE, RMSE),
      names_sep = "_Mois_",
      names_glue = "{.value}_{Period_Group}_M{month_in_quarter}" 
    ) |>
    
    #Changement de nom de variable
    mutate(
      Model = model_name) |>
    
    # SÉLECTION ET TRI DES COLONNES DE SORTIE
    select(Model, starts_with("MAE"), starts_with("RMSE"))
  
  return(final_wide)
}

############
#Exécution sur tous les modèles
############

# Liste des fichiers
files_list <- list(
  # --- Modèles BDF ---
  
  "BDF_txt" ="Final_results/BDF_text_2020.xlsx",
  "BDF_txtrol" ="Final_results/BDF_rolling_text_2020.xlsx", 
  "BDF_txtO" ="Final_results/BDF_just_text_2020.xlsx", 
  "BDF_txtTS" ="Final_results/BDF_all_2020.xlsx", 
  "BDF_TS" ="Final_results/BDF_excel_2020.xlsx", 
  "BDF_txtFR" ="Final_results/BDF_text_FR_2020.xlsx", 
  
  
  # --- Modèles INSEE ---
  
  "INSEE_txt" ="Final_results/INSEE_text_2020.xlsx",
  "INSEE_notxt" ="Final_results/INSEE_noText_2020.xlsx", 
  "INSEE_txtrol" ="Final_results/INSEE_rolling_text_2020.xlsx", 
 "INSEE_txtO" ="Final_results/INSEE_just_text_2020.xlsx", 
  "INSEE_txtTS" ="Final_results/INSEE_all_2020.xlsx", 
  "INSEE_TS" ="Final_results/INSEE_excel_2020.xlsx", 
  "INSEE_txtFR" ="Final_results/INSEE_text_FR_2020.xlsx", 
  
  
  # --- Modèles ECO ---
  
  "all_txt" ="Final_results/ECO_text_2020.xlsx"
  
)

# Boucle pour tout calculer
results_list <- list()

for(model in names(files_list)) {
  print(paste("Traitement de :", model))
  path <- files_list[[model]]
  
  try({
    results_list[[model]] <- analyze_period_monthly_accuracy(path, model, pib, COVID)
  }, silent = TRUE)
}

# Tableau final
final_period_monthly_analysis <- bind_rows(results_list)

write.xlsx(final_period_monthly_analysis, "Analysis_monthy_period.xlsx")



