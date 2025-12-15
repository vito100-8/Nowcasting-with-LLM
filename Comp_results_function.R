# #############################################################
# ANALYSE ET COMPARAISON DES MODÈLES 
# ###########################################################

rm(list = ls()) 
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Cutoff_perf_analysis.R")
source("LLM_MAE_RMSE_function.R")
source("Cutoff_perf_analysis.R") 

#Inputs/Paramètres
MODEL_NAME_COMP <- " Text"
FILE_PATH_BDF   <- "Final_results/BDF_text_2020.xlsx" 
FILE_PATH_INSEE <- "Final_results/INSEE_text_2020.xlsx" 

# Couleurs pour les graphiques
CUSTOM_COLORS <- c(
  "LLM BDF" = "cornflowerblue", 
  "LLM INSEE" = "cyan4", 
  "LLM ALL" = "cyan", 
  "ECONOMETRICS" = "coral1" 
)

# Liste des modèles standards (pour les graphiques "résumés")
MODELS_STD <- c("BDF_txt", "INSEE_txt", "ALL_txt", "all_txt", "ECO_ALL", "ECO_BDF", "ECO_INSEE")

##################################
#FONCTIONS UTILITAIRES
##################################

#RMSE des modèles sur la période 
calculate_eco_rmse <- function(df_eco_filtered, specific_ind = FALSE) {
  
  calc_rmse <- function(df, model_name, prefix) {
    df |> na.omit() |> summarise(
      Model = model_name,
      RMSE_M1 = sqrt(mean((PIB_PR - get(paste0(prefix, "_M1")))^2)),
      RMSE_M2 = sqrt(mean((PIB_PR - get(paste0(prefix, "_M2")))^2)),
      RMSE_M3 = sqrt(mean((PIB_PR - get(paste0(prefix, "_M3")))^2))
    )
  }
  
  df_bdf   <- df_eco_filtered |> select(PIB_PR, starts_with("BDF_ALL"))
  df_insee <- df_eco_filtered |> select(PIB_PR, starts_with("INSEE_ALL"))
  df_comb  <- df_eco_filtered |> select(PIB_PR, starts_with("COMB_ALL"))
  
  res <- bind_rows(
    calc_rmse(df_bdf, "ECO_BDF", "BDF_ALL"),
    calc_rmse(df_insee, "ECO_INSEE", "INSEE_ALL"),
    calc_rmse(df_comb, "ECO_ALL", "COMB_ALL")
  )
  
  if(specific_ind) {
    df_ar        <- df_eco_filtered |> select(PIB_PR, starts_with("AR"))
    df_bdf_ind   <- df_eco_filtered |> select(PIB_PR, starts_with("BDF_IND"))
    df_insee_ind <- df_eco_filtered |> select(PIB_PR, starts_with("INSEE_IND"))
    
    res_ind <- bind_rows(
      calc_rmse(df_ar, "ECO_AR", "AR"),
      calc_rmse(df_bdf_ind, "ECO_BDF_IND", "BDF_IND"),
      calc_rmse(df_insee_ind, "ECO_INSEE_IND", "INSEE_IND")
    )
    res <- bind_rows(res, res_ind)
  }
  return(res)
}

#Prépare rank des RMSE
plot_ranking_rmse <- function(metrics_data, title_plot, subtitle_plot, target_models = NULL) {
  
  df_plot <- metrics_data
  
  if (!is.null(target_models)) {
    df_plot <- df_plot |> filter(Model %in% target_models)
  }
  
  df_plot <- df_plot |>
    pivot_longer(cols = starts_with("RMSE"), names_to = "Mois", values_to = "RMSE") |>
    mutate(
      Model = ifelse(Model == "all_txt", "ALL_txt", Model),
      Type = case_when(
        str_detect(Model, "^BDF") ~ "LLM BDF",
        str_detect(Model, "^INSEE") ~ "LLM INSEE",
        str_detect(Model, "^ECO") ~ "ECONOMETRICS",
        TRUE ~ "LLM ALL"
      ),
      Mois = str_replace(Mois, "RMSE_", "")
    ) |>
    group_by(Mois) |>
    mutate(Model_Ordered = reorder_within(Model, -RMSE, Mois)) |>
    ungroup()
  
  p <- ggplot(df_plot, aes(x = RMSE, y = Model_Ordered, fill = Type)) +
    geom_col(width = 0.7, alpha = 0.9) +
    geom_text(aes(label = round(RMSE, 3)), hjust = -0.2, size = 3.5, fontface = "bold") +
    facet_wrap(~Mois, scales = "free_y", ncol = 3) +
    scale_y_reordered() +
    scale_fill_manual(values = CUSTOM_COLORS) +
    labs(title = title_plot, subtitle = subtitle_plot, x = "RMSE", y = "", fill = "Model") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.background = element_rect(fill = "#2C3E50", color = NA),
      strip.text = element_text(color = "white", face = "bold", size = 10),
      panel.grid.major.y = element_blank(),
      legend.position = "top"
    ) +
    expand_limits(x = max(df_plot$RMSE) * 1.15)
  
  print(p)
}

##################################
# ANALYSE COMPARATIVE DÉTAILLÉE BDF vs INSEE
##################################


message("Statistiques descriptives BDF vs INSEE")

prep_stats <- function(path, src) {
  read_xlsx(path) |> rowwise() |> 
    mutate(median_forecast = median(c_across(starts_with("forecast_")), na.rm=T),
           median_conf = median(c_across(starts_with("confidence_")), na.rm=T), source=src) |>
    select(Date, median_forecast, median_conf, source) |> ungroup()
}

stats_both <- bind_rows(prep_stats(FILE_PATH_BDF, "BDF"), prep_stats(FILE_PATH_INSEE, "INSEE")) |>
  mutate(month = month(Date), year = year(Date),
         month_in_quarter = case_when(month %in% c(2,5,8,11)~1, month %in% c(3,6,9,12)~2, TRUE~3),
         Mois_Label = paste0("M", month_in_quarter),
         forecast_year = ifelse(month==1, year-1, year),
         forecast_quarter = case_when(month %in% c(2,3,4)~1, month %in% c(5,6,7)~2, month %in% c(8,9,10)~3, TRUE~4))

# Rankings des modeles
df_ranked <- final_period_monthly_analysis |> 
  mutate(across(c(starts_with("MAE"), starts_with("RMSE")), rank)) |>
  pivot_longer(cols=-Model, names_to="Metrique", values_to="Rang")

print(ggplot(df_ranked, aes(x=Metrique, y=Model, fill=Rang)) + geom_tile(color="white") +
        geom_text(aes(label=Rang), color="white", fontface="bold") + 
        scale_fill_gradient(low="#2E86C1", high="#D6EAF8") + theme_minimal() +
        labs(title="Classement des modèles") + theme(axis.text.x=element_text(angle=45, hjust=1)))


##################################
#CLASSEMENTS RMSE 
##################################
message("Génération des graphiques de ranking")

# Chargement données Éco 
df_eco_global <- read_xlsx("./Results_Eco_Climat.xlsx") |> 
  filter(dates > "2015-01-01")

#  Période Globale (2015-2025) 
rmse_eco_global <- calculate_eco_rmse(df_eco_global, specific_ind = FALSE) |>
  rename_with(.fn = ~ gsub("RMSE_Mois_", "RMSE_M", .), .cols = starts_with("RMSE_Mois_"))

metrics_llm_global <- read_xlsx("metrics_covid.xlsx") |> select(Model, starts_with("RMSE")) |>
  rename_with(.fn = ~ gsub("RMSE_Mois_", "RMSE_M", .), .cols = starts_with("RMSE_Mois_"))

metrics_final_global <- bind_rows(rmse_eco_global, metrics_llm_global)

# Graphique
plot_ranking_rmse(metrics_final_global, "Models ranking based on RMSE", "2015-2025", MODELS_STD)

# Période sans COVID
##  Modèles Éco 
df_eco_nocovid <- df_eco_global |>
  filter(dates < "2020-02-01" | dates > "2022-01-01")
rmse_eco_nocovid <- calculate_eco_rmse(df_eco_nocovid, specific_ind = FALSE) |>
  rename_with(.fn = ~ gsub("RMSE_Mois_", "RMSE_M", .), .cols = starts_with("RMSE_Mois_"))

##  Modèles LLM 
metrics_llm_nocovid <- read_xlsx("metrics_no_covid.xlsx") |>
  select(Model, starts_with("RMSE")) |>
  rename_with(.fn = ~ gsub("RMSE_Mois_", "RMSE_M", .), .cols = starts_with("RMSE_Mois_"))

metrics_final_nocovid <- bind_rows(rmse_eco_nocovid, metrics_llm_nocovid)

plot_ranking_rmse(metrics_final_nocovid,  "Models ranking based on RMSE", "2015-2025 without the COVID period", MODELS_STD)


#  Sous-Période 2015-2019 ---
df_eco_1519 <- df_eco_global |> filter(dates < "2020-02-01")
rmse_eco_1519 <- calculate_eco_rmse(df_eco_1519) |>
  rename_with(.fn = ~ gsub("RMSE_Mois_", "RMSE_M", .), .cols = starts_with("RMSE_Mois_"))

metrics_llm_1519 <- final_period_monthly_analysis |>
  select(Model, starts_with("RMSE_2015")) |>
  rename_with(.fn = ~ gsub("RMSE_2015_2019_M", "RMSE_M", .), .cols = starts_with("RMSE_2015"))

metrics_final_1519 <- bind_rows(rmse_eco_1519, metrics_llm_1519)
plot_ranking_rmse(metrics_final_1519, "Models ranking based on RMSE", "2015-2019", MODELS_STD)


#  Sous-Période 2020-2025 
df_eco_2025 <- df_eco_global |> filter(dates >= "2020-02-01")
rmse_eco_2025 <- calculate_eco_rmse(df_eco_2025) |>
  rename_with(.fn = ~ gsub("RMSE_Mois_", "RMSE_M", .), .cols = starts_with("RMSE_Mois_"))

metrics_llm_2025 <- final_period_monthly_analysis |>
  select(Model, starts_with("RMSE_2020")) |>
  rename_with(.fn = ~ gsub("RMSE_2020_2025_M", "RMSE_M", .), .cols = starts_with("RMSE_2020"))

metrics_final_2025 <- bind_rows(rmse_eco_2025, metrics_llm_2025)
plot_ranking_rmse(metrics_final_2025, "Models ranking based on RMSE", "2020-2025", MODELS_STD)


# Période Globale (ALL LLM + ECO)
metrics_final_all_llm <- bind_rows(rmse_eco_global, metrics_llm_global)

plot_ranking_rmse(
  metrics_data = metrics_final_all_llm, 
  title_plot = "Models ranking based on RMSE", 
  subtitle_plot = "2015-2025", 
  target_models = NULL 
)


# Période Sans Covid (ALL LLM +  Eco ) 
##  Modèles Éco 
df_eco_nocovid <- df_eco_global |>
  filter(dates < "2020-02-01" | dates > "2022-01-01")
rmse_eco_nocovid <- calculate_eco_rmse(df_eco_nocovid, specific_ind = FALSE) |>
  rename_with(.fn = ~ gsub("RMSE_Mois_", "RMSE_M", .), .cols = starts_with("RMSE_Mois_"))

##  Modèles LLM 
metrics_llm_nocovid <- read_xlsx("metrics_no_covid.xlsx") |>
  select(Model, starts_with("RMSE")) |>
  rename_with(.fn = ~ gsub("RMSE_Mois_", "RMSE_M", .), .cols = starts_with("RMSE_Mois_"))

metrics_final_nocovid <- bind_rows(rmse_eco_nocovid, metrics_llm_nocovid)

plot_ranking_rmse(
  metrics_data = metrics_final_nocovid, 
  title_plot = "Models ranking based on RMSE", 
  subtitle_plot = "2015-2025 without the COVID period", 
  target_models = NULL 
)


# #################################
# SÉRIE TEMPORELLE
# #################################
message("Time serie")

df_PIB_clean <- read_xlsx("Data_BDF_INSEE.xlsx", sheet="trimestriel") |>
  mutate(Date = as.Date(dates), forecast_year = year(Date), Month = month(Date),
         forecast_quarter = case_when(Month %in% c(2,5,8,11)~floor(Month/3)+1, TRUE~NA_real_)) |>
  select(forecast_year, forecast_quarter, PIB_PR, Date)

serie_temp <- left_join(stats_both, df_PIB_clean, by = c("forecast_year", "forecast_quarter")) |>
  rename(Model = source) |>
  pivot_longer(cols = c(median_forecast, PIB_PR), names_to = "Metric", values_to = "Value") |>
  mutate(Model = ifelse(Metric == "PIB_PR", "PIB_PR", Model)) |>
  filter((Metric == "PIB_PR") | (Model %in% c("BDF", "INSEE") & Metric == "median_forecast")) |>
  mutate(
    Line_Type = case_when(
      Model == "PIB_PR" ~ "GDP growth",
      Model == "BDF" ~ "Nowcasts BDF",
      Model == "INSEE" ~ "Nowcasts INSEE",
      TRUE ~ "Autre"
    )
  )

print(ggplot(serie_temp, aes(y = Value, x = as.Date(Date.x), color = Line_Type)) +
        ylim(-0.25, 0.75) + 
        geom_line(linewidth = 0.75) +
        scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = expansion(mult = 0.02)) +
        geom_rect(xmin = as.Date("2020-01-01"), xmax = as.Date("2022-01-01"), 
                  ymin = -Inf, ymax = Inf, fill = "gray80", alpha = 0.4, color = NA) +
        scale_color_manual(name = "Source", 
                           values = c("GDP growth" = "black", "Nowcasts BDF" = "blue", "Nowcasts INSEE" = "red")) +
        labs(title = "Forecast vs Actual GDP growth", y = "GDP growth (%)", x = "YEAR") + 
        theme_minimal())

message("Terminé !")