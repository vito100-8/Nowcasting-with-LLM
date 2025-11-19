# Statistiques Descriptives Resultats

rm(list = ls())  
source("Library_Nowcasting_LLM.R")


#########################
#Cutoff date et COVID
##########################

cutoff <- as.Date("2025-01-01") # à modifier si évolution

COVID <- 0 # 0 = on ne prend pas la période covid 1 = on la considère

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
      Abs_Error = abs(PIB_PR - median_f),
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
      RMSE = sqrt(mean(Abs_Error^2, na.rm = TRUE)),
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
  "BDF_text" = "Final_results/BDF_text_2020.xlsx",
  "BDF_noText" = "Final_results/BDF_noText_2020.xlsx",
  "BDF_rolling" = "Final_results/BDF_rolling_text_2020.xlsx",
  "BDF_just" = "Final_results/BDF_just_text_2020.xlsx",
  "BDF_all" = "Final_results/BDF_all_2020.xlsx",
  "BDF_excel" = "Final_results/BDF_excel_2020.xlsx",
  "BDF_excel_error" = "Final_results/BDF_excel_error_2020.xlsx",
  
  "INSEE_text" = "Final_results/INSEE_text_2020.xlsx",
  "INSEE_noText" = "Final_results/INSEE_noText_2020.xlsx",
  "INSEE_rolling" = "Final_results/INSEE_rolling_text_2020.xlsx",
  "INSEE_just" = "Final_results/INSEE_just_text_2020.xlsx",
  "INSEE_all" = "Final_results/INSEE_all_2020.xlsx",
  "INSEE_excel" =  "Final_results/INSEE_excel_2020.xlsx",
  "INSEE_excel_error" = "Final_results/INSEE_excel_error_2020.xlsx"
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





###################
# Split periods
#####################


#2 périodes de 5 ans
cutoff_1 <- as.Date("2020-01-01")



####################################
#Fonction analyse cutoff
###########################
analyze_period_accuracy <- function(file_path, model_name, pib_data, cutoff_date) {
  
  
  df_model <- read_xlsx(file_path)
  
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
      Abs_Error = abs(PIB_PR - median_f),
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
      RMSE = sqrt(mean(Abs_Error^2, na.rm = TRUE)),
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

# Liste de vos fichiers
files_list <- list(
  "BDF_text" = "Final_results/BDF_text_2020.xlsx",
  "BDF_noText" = "Final_results/BDF_noText_2020.xlsx",
  "BDF_rolling" = "Final_results/BDF_rolling_text_2020.xlsx",
  "BDF_just" = "Final_results/BDF_just_text_2020.xlsx",
  "BDF_all" = "Final_results/BDF_all_2020.xlsx",
  "BDF_excel" = "Final_results/BDF_excel_2020.xlsx",
  "BDF_excel_error" = "Final_results/BDF_excel_error_2020.xlsx",
  
  "INSEE_text" = "Final_results/INSEE_text_2020.xlsx",
  "INSEE_noText" = "Final_results/INSEE_noText_2020.xlsx",
  "INSEE_rolling" = "Final_results/INSEE_rolling_text_2020.xlsx",
  "INSEE_just" = "Final_results/INSEE_just_text_2020.xlsx",
  "INSEE_all" = "Final_results/INSEE_all_2020.xlsx",
  "INSEE_excel" =  "Final_results/INSEE_excel_2020.xlsx",
  "INSEE_excel_error" = "Final_results/INSEE_excel_error_2020.xlsx"
)

# Boucle pour tout calculer
results_list <- list()

for(model in names(files_list)) {
  print(paste("Traitement de :", model))
  path <- files_list[[model]]
  
  try({
    results_list[[model]] <- analyze_period_accuracy(path, model, pib, cutoff_1)
  })
}

# Tableau final
final_period_analysis <- bind_rows(results_list)





### Pour export

library(gridExtra)

png("Recap_Final.png", height = 30*nrow(final_period_analysis), width = 800); grid.table(final_period_analysis); dev.off()

png("Analyse_Cutoff.png", height = 30*nrow(final_cutoff_analysis), width = 1000); grid.table(final_cutoff_analysis); dev.off()
