# Calcul MAE et RMSE de ISMA et AR(2)

source("LLM_Sentiment_AR.R")
source("LLM_AR_climat.R")

##################
# COVID
###############

COVID <- 1 # 1 = on supprime la période COVID; 0 = on garde
if (COVID == 1){
  df_AR_Clim <- Results_Forecast_Clim |> 
    filter(dates < "2020-02-01" | dates >= "2022-02-01")
  df_AR_Sent <- Results_Forecast_Sent |> 
    filter(dates < "2020-02-01" | dates >= "2022-02-01")
}else {
  df_AR_Clim <- Results_Forecast_Clim
  df_AR_Sent <- Results_Forecast_Sent 
  
}

#################################
# Calcul MAE/RMSE
#################################



# Indice de sentiment BDF 
metrics_BDF_AR_Sent <- df_AR_Sent |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Sentiment BDF",
    MAE_M1 = mean(abs(PIB_PR - Prev_BDF_SENT_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - Prev_BDF_SENT_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - Prev_BDF_SENT_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M3)^2, na.rm = TRUE))
  )

#Indice de sentiment INSEE
metrics_INSEE_AR_Sent <- df_AR_Sent |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Sentiment INSEE",
    MAE_M1 = mean(abs(PIB_PR - Prev_INSEE_SENT_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - Prev_INSEE_SENT_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - Prev_INSEE_SENT_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M3)^2, na.rm = TRUE))
  )



# BDF Modèle 1 (Industrie Seule)
metrics_AR_Clim_BDF_1 <- df_AR_Clim |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat BDF (Ind)",
    MAE_M1 = mean(abs(PIB_PR - BDF_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - BDF_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - BDF_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - BDF_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - BDF_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - BDF_IND_M3)^2, na.rm = TRUE))
  )

#  BDF Modèle 2 (Industrie + Services)
metrics_AR_Clim_BDF_2 <- df_AR_Clim |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat BDF (All)",
    MAE_M1 = mean(abs(PIB_PR - BDF_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - BDF_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - BDF_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - BDF_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - BDF_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - BDF_ALL_M3)^2, na.rm = TRUE))
  )

# INSEE Modèle 1 (Industrie Seule)
metrics_Clim_INSEE_1 <- df_AR_Clim |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat INSEE (Ind)",
    MAE_M1 = mean(abs(PIB_PR - INSEE_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - INSEE_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - INSEE_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - INSEE_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - INSEE_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - INSEE_IND_M3)^2, na.rm = TRUE))
  )

#  INSEE Modèle 2 (Industrie + Services + Bâtiment)
metrics_Clim_INSEE_2 <- df_AR_Clim |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat INSEE (All)",
    MAE_M1 = mean(abs(PIB_PR - INSEE_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - INSEE_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - INSEE_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - INSEE_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - INSEE_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - INSEE_ALL_M3)^2, na.rm = TRUE))
  )

#INSEE ET BDF Industrie
metrics_Clim_comb_1 <- df_AR_Clim|>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat INSEE/BDF (IND)",
    MAE_M1 = mean(abs(PIB_PR - COMB_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - COMB_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - COMB_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - COMB_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - COMB_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - COMB_IND_M3)^2, na.rm = TRUE))
  )


#INSEE et BDF tous les climats
metrics_Clim_comb_2 <- df_AR_Clim |>
  filter(dates >= "2010-02-01") |>
  summarise(
    Model = "Climat INSEE/BDF (All)",
    MAE_M1 = mean(abs(PIB_PR - COMB_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - COMB_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - COMB_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - COMB_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - COMB_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - COMB_ALL_M3)^2, na.rm = TRUE))
  )


##############################
#Tableau recap
##################


econ_metrics_table <- bind_rows( metrics_BDF_AR_Sent, metrics_INSEE_AR_Sent, metrics_AR_Clim_BDF_1, 
                                metrics_AR_Clim_BDF_2, metrics_Clim_INSEE_1, metrics_Clim_INSEE_2 ,
                                metrics_Clim_comb_1, metrics_Clim_comb_2) |>
  pivot_longer(
    cols = -Model, 
    names_to = c(".value", "Mois"),
    names_pattern = "(MAE|RMSE)_M([1-3])", 
  ) |>
  pivot_wider(
    names_from = Mois, 
    values_from = c(MAE, RMSE),
    names_glue = "{.value}_M{Mois}"
  ) |>
  select(Model, starts_with("MAE"), starts_with("RMSE"))




###################################
# Calcul MAE/RMSE en période
#################################

# cutoff
cutoff_date <- as.Date("2020-02-01")

#Indice de Sentiment BDF
metrics_Sent_BDF_period <- df_AR_Sent |>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Sentiment BDF ",
    MAE_M1 = mean(abs(PIB_PR - Prev_BDF_SENT_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - Prev_BDF_SENT_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - Prev_BDF_SENT_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - Prev_BDF_SENT_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

#Indice de sentiment INSEE
metrics_Sent_INSEE_period <- df_AR_Sent |>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Sentiment INSEE",
    MAE_M1 = mean(abs(PIB_PR - Prev_INSEE_SENT_M1 ), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - Prev_INSEE_SENT_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - Prev_INSEE_SENT_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - Prev_INSEE_SENT_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )


# Climat BDF Modèle 1 (Industrie)
metrics_Clim_BDF_1_period <- df_AR_Clim |>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat BDF (Ind)",
    MAE_M1 = mean(abs(PIB_PR - BDF_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - BDF_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - BDF_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - BDF_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - BDF_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - BDF_IND_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

# Climat BDF Modèle 2 (Industrie + Services)
metrics_Clim_BDF_2_period <- df_AR_Clim|>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat BDF (All)",
    MAE_M1 = mean(abs(PIB_PR - BDF_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - BDF_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - BDF_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - BDF_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - BDF_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - BDF_ALL_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

# Climat INSEE Modèle 1 (Industrie)
metrics_Clim_INSEE_1_period <- df_AR_Clim|>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat INSEE (Ind)",
    MAE_M1 = mean(abs(PIB_PR - INSEE_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - INSEE_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - INSEE_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - INSEE_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - INSEE_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - INSEE_IND_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

# Climat INSEE Modèle 2 (Tout)
metrics_Clim_INSEE_2_period <- df_AR_Clim|>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat INSEE (All)",
    MAE_M1 = mean(abs(PIB_PR - INSEE_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - INSEE_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - INSEE_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - INSEE_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - INSEE_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - INSEE_ALL_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

#INSEE/BDF INDUSTRIE
metrics_Clim_comb_1_period <- df_AR_Clim|>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat INSEE (All)",
    MAE_M1 = mean(abs(PIB_PR - COMB_IND_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - COMB_IND_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - COMB_IND_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - COMB_IND_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - COMB_IND_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - COMB_IND_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )

#INSEE/BDF ALL
metrics_Clim_comb_2_period <- df_AR_Clim |>
  filter(dates >= "2015-02-01") |>
  mutate(Period = case_when(dates < cutoff_date ~ "2015_2019", dates >= cutoff_date ~ "2020_2025")) |>
  group_by(Period) |>
  summarise(
    Model = "Climat INSEE (All)",
    MAE_M1 = mean(abs(PIB_PR - COMB_ALL_M1), na.rm = TRUE),
    MAE_M2 = mean(abs(PIB_PR - COMB_ALL_M2), na.rm = TRUE),
    MAE_M3 = mean(abs(PIB_PR - COMB_ALL_M3), na.rm = TRUE),
    RMSE_M1 = sqrt(mean((PIB_PR - COMB_ALL_M1)^2, na.rm = TRUE)),
    RMSE_M2 = sqrt(mean((PIB_PR - COMB_ALL_M2)^2, na.rm = TRUE)),
    RMSE_M3 = sqrt(mean((PIB_PR - COMB_ALL_M3)^2, na.rm = TRUE)),
    .groups = "drop"
  )


#TABLEAU RECAP FINAL
econ_metrics_period <- bind_rows(
  metrics_Sent_BDF_period,
  metrics_Sent_INSEE_period,
  metrics_Clim_BDF_1_period,
  metrics_Clim_BDF_2_period,
  metrics_Clim_INSEE_1_period,
  metrics_Clim_INSEE_2_period,
  metrics_Clim_comb_1_period,
  metrics_Clim_comb_2_period
  
)

##################################################################################
# Analyse comparative graphique
#################################################################################

# Code couleur
CUSTOM_COLORS <- c(
  "Sentiment" = "#2E86C1",
  "Climate"   = "#E67E22"  
)

# Même type de fonction pour le graphique que dans comp_results_function
plot_ranking_rmse_clean <- function(metrics_data, title_plot, subtitle_plot) {
  
  df_plot <- metrics_data |>
    pivot_longer(cols = starts_with("RMSE"), names_to = "Mois", values_to = "RMSE") |>
    mutate(
      # Définition des deux catégorie
      Category = ifelse(str_detect(Model, "Sentiment"), "Sentiment", "Climate"),
      
      # Nettoyer les noms
      Model_Clean = Model,
      Model_Clean = str_remove_all(Model_Clean, "(?i)sentiment|climate|climat"),
      
      # Uniformisation IND/ALL
      Model_Clean = str_replace_all(Model_Clean, "(?i)ind", "IND"),
      Model_Clean = str_replace_all(Model_Clean, "(?i)all", "ALL"),
      
      
      # Label Mois
      Mois = str_replace(Mois, "RMSE_", "")
    ) |>
    group_by(Mois) |>
    mutate(Model_Ordered = reorder_within(Model_Clean, -RMSE, Mois)) |>
    ungroup()
  
  #graphique
  ggplot(df_plot, aes(x = RMSE, y = Model_Ordered, fill = Category)) +
    geom_col(width = 0.75, alpha = 0.85) +
    
    # Paramètres valeurs affichées
    geom_text(aes(label = round(RMSE, 3)), 
              hjust = -0.15, size = 3.2, fontface = "bold", color = "grey20") +
    
    # Facet
    facet_wrap(~Mois, scales = "free_y", ncol = 3) +
    
    scale_y_reordered() +
    scale_fill_manual(values = CUSTOM_COLORS) +
    
    labs(
      title = title_plot, 
      subtitle = subtitle_plot, 
      x = "RMSE", 
      y = "", 
      fill = "Model Type"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.background = element_rect(fill = "#34495E", color = NA),
      strip.text = element_text(color = "white", face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box.margin = margin(t = 10)
    ) +
    expand_limits(x = max(df_plot$RMSE) * 1.2)
}

# Appel de la fonction selon dummy covid
if (COVID == 0) {
  plot_ranking_rmse_clean(
    metrics_data = econ_metrics_table, 
    title_plot = "Model ranking : Climate vs LLM sentiment ",
    subtitle_plot = "2015-2025 including COVID period"
  )
} else {
  plot_ranking_rmse_clean(
    metrics_data = econ_metrics_table, 
    title_plot = "Model ranking : Climate vs LLM sentiment ",
    subtitle_plot = "2015-2025 excluding COVID period"
  )
}
