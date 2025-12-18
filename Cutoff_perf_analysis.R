# Statistiques Descriptives Resultats

source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")

#########################
# Cutoff date et COVID
##########################

cutoff <- as.Date("2025-01-01") # à modifier si évolution

COVID <- 1 # 0 = on ne prend pas la période covid;  1 = si

################################################
# PRÉPARATION DES DONNÉES DE PIB
################################################

df_PIB <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")

# Nettoyage du PIB
pib <- df_PIB |>
  mutate(
    Date = as.Date(dates),
    forecast_year = year(Date),
    Month = month(Date),
    forecast_quarter = case_when(
      Month == 2 ~ 1,
      Month == 5 ~ 2,
      Month == 8 ~ 3,
      Month == 11 ~ 4
    )
  )

############
# Exécution sur tous les modèles
############

# Liste des modèles
files_list <- list(
  # --- Modèles BDF ---
  "BDF_txt" = "Final_results/BDF_text_2020.xlsx",
  "BDF_txtrol" = "Final_results/BDF_rolling_text_2020.xlsx",
  "BDF_txtO" = "Final_results/BDF_just_text_2020.xlsx",
  "BDF_txtTS" = "Final_results/BDF_all_2020.xlsx",
  "BDF_TS" = "Final_results/BDF_excel_2020.xlsx",
  "BDF_txtFR" = "Final_results/BDF_text_FR_2020.xlsx",


  # --- Modèles INSEE ---

  "INSEE_txt" = "Final_results/INSEE_text_2020.xlsx",
  "INSEE_notxt" = "Final_results/INSEE_noText_2020.xlsx",
  "INSEE_txtrol" = "Final_results/INSEE_rolling_text_2020.xlsx",
  "INSEE_txtO" = "Final_results/INSEE_just_text_2020.xlsx",
  "INSEE_txtTS" = "Final_results/INSEE_all_2020.xlsx",
  "INSEE_TS" = "Final_results/INSEE_excel_2020.xlsx",
  "INSEE_txtFR" = "Final_results/INSEE_text_FR_2020.xlsx",


  # --- Modèles ECO ---

  "all_txt" = "Final_results/ECO_text_2020.xlsx"
)

# Boucle pour tout calculer
results_list <- list()

for (model in names(files_list)) {
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


############
# Exécution sur tous les modèles
############

# Liste des fichiers
files_list <- list(
  # --- Modèles BDF ---
  "BDF_txt" = "Final_results/BDF_text_2020.xlsx",
  "BDF_txtrol" = "Final_results/BDF_rolling_text_2020.xlsx",
  "BDF_txtO" = "Final_results/BDF_just_text_2020.xlsx",
  "BDF_txtTS" = "Final_results/BDF_all_2020.xlsx",
  "BDF_TS" = "Final_results/BDF_excel_2020.xlsx",
  "BDF_txtFR" = "Final_results/BDF_text_FR_2020.xlsx",


  # --- Modèles INSEE ---

  "INSEE_txt" = "Final_results/INSEE_text_2020.xlsx",
  "INSEE_notxt" = "Final_results/INSEE_noText_2020.xlsx",
  "INSEE_txtrol" = "Final_results/INSEE_rolling_text_2020.xlsx",
  "INSEE_txtO" = "Final_results/INSEE_just_text_2020.xlsx",
  "INSEE_txtTS" = "Final_results/INSEE_all_2020.xlsx",
  "INSEE_TS" = "Final_results/INSEE_excel_2020.xlsx",
  "INSEE_txtFR" = "Final_results/INSEE_text_FR_2020.xlsx",


  # --- Modèles ECO ---

  "all_txt" = "Final_results/ECO_text_2020.xlsx"
)

# Boucle pour tout calculer
results_list <- list()

for (model in names(files_list)) {
  print(paste("Traitement de :", model))
  path <- files_list[[model]]

  try({
    results_list[[model]] <- analyze_period_accuracy(path, model, pib, cutoff_1, COVID)
  })
}

# Tableau final
final_period_analysis <- bind_rows(results_list)

write.xlsx(final_period_analysis, "Analysis_Period.xlsx")


############
# Exécution sur tous les modèles
############

# Liste des fichiers
files_list <- list(
  # --- Modèles BDF ---
  "BDF_txt" = "Final_results/BDF_text_2020.xlsx",
  "BDF_txtrol" = "Final_results/BDF_rolling_text_2020.xlsx",
  "BDF_txtO" = "Final_results/BDF_just_text_2020.xlsx",
  "BDF_txtTS" = "Final_results/BDF_all_2020.xlsx",
  "BDF_TS" = "Final_results/BDF_excel_2020.xlsx",
  "BDF_txtFR" = "Final_results/BDF_text_FR_2020.xlsx",


  # --- Modèles INSEE ---

  "INSEE_txt" = "Final_results/INSEE_text_2020.xlsx",
  "INSEE_notxt" = "Final_results/INSEE_noText_2020.xlsx",
  "INSEE_txtrol" = "Final_results/INSEE_rolling_text_2020.xlsx",
  "INSEE_txtO" = "Final_results/INSEE_just_text_2020.xlsx",
  "INSEE_txtTS" = "Final_results/INSEE_all_2020.xlsx",
  "INSEE_TS" = "Final_results/INSEE_excel_2020.xlsx",
  "INSEE_txtFR" = "Final_results/INSEE_text_FR_2020.xlsx",


  # --- Modèles ECO ---

  "all_txt" = "Final_results/ECO_text_2020.xlsx"
)

# Boucle pour tout calculer
results_list <- list()

for (model in names(files_list)) {
  print(paste("Traitement de :", model))
  path <- files_list[[model]]

  try(
    {
      results_list[[model]] <- analyze_period_monthly_accuracy(path, model, pib, COVID)
    },
    silent = TRUE
  )
}

# Tableau final
final_period_monthly_analysis <- bind_rows(results_list)

write.xlsx(final_period_monthly_analysis, "Analysis_monthy_period.xlsx")
