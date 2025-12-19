# Résultats et analyse de résultats des combinaisons de forecasts
################################################################################

source("LLM_for_comb_function.R")

#Avec le réglage window = 20 la période va de 2020 à 2025

################################################################################
# CONSOLIDATION ET NETTOYAGE DES RÉSULTATS
################################################################################

# Modèles de combinaisons
all_results_objects <- list(
  "BDF+INSEE"       = res_BDF_INSEE_Text,
  "BDF+Climat"      = res_BDF_Text_Clim,
  "All BDF"         = res_BDF,
  "All INSEE"       = res_INSEE,
  "All ECO"         = res_ECO,
  "All LLM"         = res_LLM,
  "All models"      = res_ALL
)


# df avec tous les résultats
df_master <- bind_rows(lapply(names(all_results_objects), function(x) extract_data_clean(x, all_results_objects[[x]])))

# Noms d'affichage et enlever les doublons des modèles seuls
df_clean <- df_master |>
  mutate(
    # Type de combinaison
    Type = ifelse(Modele_Raw %in% c("AVG", "INV", "GR", "CONF"), "Combinaison", "Modele_Seul"),

    # Standardisation des noms
    Display_Name = case_when(
      Type == "Combinaison" ~ paste0(Modele_Raw, " [", Scenario, "]"),

      # Si modèles seules avec noms répétitifs
      Scenario == "Tous BDF" & !grepl("^BDF_", Modele_Raw) ~ paste0("BDF_", Modele_Raw),
      Scenario == "Tous INSEE" & !grepl("^INSEE_", Modele_Raw) ~ paste0("INSEE_", Modele_Raw),
      Scenario == "Tous LLM" & !grepl("^INSEE_", Modele_Raw) ~ paste0("INSEE_", Modele_Raw),
      Scenario == "Tous LLM" & !grepl("^BDF", Modele_Raw) ~ paste0("BDF_", Modele_Raw),


      # Expliciter le nom bdf
      Modele_Raw == "BDF" ~ "BDF_Text",
      Modele_Raw == "INSEE" ~ "INSEE_Text",

      # Sinon on garde le nom
      TRUE ~ Modele_Raw
    )
  ) |>
  # Enlever les doublons
  group_by(Periode, Mois, Display_Name) |>
  slice_min(RMSE, n = 1, with_ties = FALSE) |>
  ungroup()



################################################################################
# Analyse comparative Combinaison vs Modèle Seul
################################################################################

# créer le df
df_synergy_prep <- df_master |>
  rename(Month = Mois) |>
  mutate(
    Month = gsub("Mois_", "M", Month),
    Modele = Modele_Raw,
    Type = ifelse(Modele %in% c("AVG", "INV", "GR", "CONF"), "Combinaison", "Modele_Seul")
  )

# Calcul des meilleurs modèles et des modèles médians
df_synergy <- df_synergy_prep |>
  filter(Periode == "Global") |>
  group_by(Scenario, Month) |>
  summarise(
    # RMSE Médian des modèles seuls
    Median_RMSE_Single = median(RMSE[Type == "Modele_Seul"], na.rm = TRUE),
    # RMSE Médian des méthodes de combinaison
    Median_RMSE_Combo = median(RMSE[Type == "Combinaison"], na.rm = TRUE),
    # Meilleur Modèle Seul
    Best_Single_RMSE = min(RMSE[Type == "Modele_Seul"], na.rm = TRUE),
    `Best Model` = Modele[Type == "Modele_Seul"][which.min(RMSE[Type == "Modele_Seul"])],
    # Meilleure Combinaison
    Best_Combo_RMSE = min(RMSE[Type == "Combinaison"], na.rm = TRUE),
    `Best Combination` = Modele[Type == "Combinaison"][which.min(RMSE[Type == "Combinaison"])],
    .groups = "drop"
  ) |>
  mutate(
    `% Gain Median` = (Median_RMSE_Single - Median_RMSE_Combo) / Median_RMSE_Combo,
    `% Gain Best` = (Best_Single_RMSE - Best_Combo_RMSE) / Best_Combo_RMSE,
  )


# Graphique
ggplot(df_synergy, aes(y = Scenario)) +
  geom_segment(aes(x = Best_Single_RMSE, xend = Best_Combo_RMSE, y = Scenario, yend = Scenario), color = "grey70") +

  # Point Rouge : Meilleur Modèle Seul
  geom_point(aes(x = Best_Single_RMSE, color = "Best Model"), size = 3.5) +

  # Point Vert : Meilleure Combinaison
  geom_point(aes(x = Best_Combo_RMSE, color = "Best Combination"), size = 3.5) +

  # Séparation par Mois
  facet_wrap(~Month, scales = "free_x") +

  # Légende
  scale_color_manual(values = c("Best Model" = "#E74C3C", "Best Combination" = "#27AE60")) +
  labs(
    title = "Forecast vs Combination Forecast",
    x = "RMSE",
    y = "",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold")
  )

# Tableau des gains/pertes (tableau latex)
df_export <- df_synergy |>
  select(Scenario, Month, `Best Model`, `Best Combination`, `% Gain Best`, `% Gain Median`) |>
  mutate(
    `% Gain Best` = percent(`% Gain Best`, accuracy = 0.01),
    `% Gain Median` = percent(`% Gain Median`, accuracy = 0.01)
  )


## Paramètres LaTeX
caption_text <- "Accuracy gain/loss per scenario (RMSE)"
label_text <- "tab:gain_scenario"

latex_table <- df_export |>
  kable(
    format = "latex",
    caption = caption_text,
    label = label_text,
    booktabs = TRUE,
    align = c("l", "l", "l", "l", "c")
  ) |>
  kable_styling(
    full_width = FALSE
  )

# Affichage du code LaTeX
cat(latex_table)
