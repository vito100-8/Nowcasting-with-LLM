# Résultats et analyse de résultats des combinaisons de forecasts
################################################################################

source("Library_Nowcasting_LLM.R")

library(forcats) # pour trier dans l'ordre croissant dans le graphique 

################################################################################
# 1. CONSOLIDATION ET NETTOYAGE DES RÉSULTATS
################################################################################

# Modèles de combinaisons
all_results_objects <- list(
  "BDF+INSEE"       = res_BDF_INSEE_Text,
  "BDF+ISMA"        = res_BDF_Text_ISMA,
  "BDF+Climat"      = res_BDF_Text_Clim,
  "Tous BDF"        = res_BDF,
  "Tous INSEE"      = res_INSEE,
  "Tous ECO"        = res_ECO,
  "GLOBAL (Tout)"   = res_ALL
)

# Fonction d'extraction pour chaque comb
extract_data_clean <- function(scenario_name, res_obj) {
  full_df <- data.frame()
  # On garde la période 2020-2025 et post covid (2022-)
  target_periods <- intersect(names(res_obj), c("Global", "P2"))
  
  for (per in target_periods) {
    for (month in c("Mois_1", "Mois_2", "Mois_3")) {
      mat <- res_obj[[per]][[month]]
      if (!is.null(mat)) {
        df_tmp <- as.data.frame(t(mat))
        
        # On garde les colonnes qui nous intéressent
        if(ncol(df_tmp) >= 2) {
          colnames(df_tmp)[1:2] <- c("MAE", "RMSE")
        }
        
        df_tmp$Modele_Raw <- rownames(df_tmp)
        df_tmp$Scenario   <- scenario_name
        df_tmp$Periode    <- per
        df_tmp$Mois    <- month
        
        full_df <- bind_rows(full_df, df_tmp)
      }
    }
  }
  return(full_df)
}

# df avec tous les résultats
df_master <- bind_rows(lapply(names(all_results_objects), function(x) extract_data_clean(x, all_results_objects[[x]])))

# Noms d'affichage et enlever les doublons des modèles seuls
df_clean <- df_master |>
  mutate(
    # Type de combinaison
    Type = ifelse(Modele_Raw %in% c("AVG", "INV", "GR"), "Combinaison", "Modele_Seul"),
    
    # Standardisation des noms
    Display_Name = case_when(
      Type == "Combinaison" ~ paste0(Modele_Raw, " [", Scenario, "]"),
      
      #Si modèles seules avec noms répétitifs
      Scenario == "Tous BDF" & !grepl("^BDF_", Modele_Raw) ~ paste0("BDF_", Modele_Raw),
      Scenario == "Tous INSEE" & !grepl("^INSEE_", Modele_Raw) ~ paste0("INSEE_", Modele_Raw),
      
      #Expliciter le nom bdf
      Modele_Raw == "BDF" ~ "BDF_Text",
      Modele_Raw == "INSEE" ~ "INSEE_Text",
      
      # Sinon on garde le nom
      TRUE ~ Modele_Raw
    )
  ) |>
  #Enlever les doublons
  group_by(Periode, Mois, Display_Name) |>
  slice_min(RMSE, n=1, with_ties = FALSE) |> 
  ungroup()


################################################################################
# 2. FONCTION DE CLASSEMENT ET AFFICHAGE
################################################################################

#TOP 10 des modèles et on précise s'il s'agit d'une combinaison ou d'un modèle simple (RMSE)
plot_top10_ranking <- function(data, target_period, plot_title) {
  
  #  10 Meilleurs
  df_top10 <- data |>
    filter(Periode == target_period) |>
    group_by(Mois) |>
    arrange(RMSE) |>       # Tri croissant 
    slice_head(n = 10) |> 
    ungroup() |> 
    mutate(
      #avoir un ordre croissant entre modèles
      Unique_ID = paste0(Display_Name, "__", Mois), 
      Unique_ID = fct_reorder(Unique_ID, RMSE, .desc = TRUE)
    )
  
  # Graphique
  ggplot(df_top10, aes(x = RMSE, y = Unique_ID, fill = Type)) +
    geom_col(width = 0.7, alpha = 0.9) +
    
    # Valeurs
    geom_text(aes(label = round(RMSE, 4)), hjust = -0.1, size = 3, fontface = "bold") +
    
    # Facet par Mois
    facet_wrap(~Mois, scales = "free_y", ncol = 3) +
    
    #Détails légende
    scale_fill_manual(values = c("Combinaison" = "#E67E22", "Modele_Seul" = "#34495E")) +
    scale_y_discrete(labels = function(x) sub("__.*", "", x)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste("10 MEILLEURS MODÈLES (RMSE)" ),
      subtitle = plot_title,
      x = "RMSE",
      y = ""
    ) +
    
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.background = element_rect(fill = "#2C3E50"),
      strip.text = element_text(color = "white", face = "bold"),
      legend.position = "bottom",
      axis.text.y = element_text(size = 9, face = "bold")
    )
}

# Période 2020-2025
print(plot_top10_ranking(df_clean, "Global", "Période Globale"))

# Période Post-Covid (P2)

if ("P2" %in% unique(df_clean$Periode)) {
  print(plot_top10_ranking(df_clean, "P2", "Période Sans Covid (2022-2025)"))
}

################################################################################
# 3. COMBINAISON OU MODELE SEUL ?
################################################################################

# créer le df 
df_synergy_prep <- df_master |>
  rename(Modele = Modele_Raw) |> 
  mutate(
    Type = ifelse(Modele %in% c("AVG", "INV", "GR"), "Combinaison", "Modele_Seul")
  )

# Calcul gains/pertes de la comb (période global par mois pour le graph, par trimestre pour le df)
df_synergy <- df_synergy_prep |>
  filter(Periode == "Global") |> 
  group_by(Scenario, Mois) |>
  summarise(
    # Meilleur Modèle Seul 
    Best_Single_RMSE = min(RMSE[Type == "Modele_Seul"], na.rm = TRUE),
    Name_Best_Single = Modele[Type == "Modele_Seul"][which.min(RMSE[Type == "Modele_Seul"])],
    
    # Meilleure Combinaison 
    Best_Combo_RMSE  = min(RMSE[Type == "Combinaison"], na.rm = TRUE),
    Name_Best_Combo  = Modele[Type == "Combinaison"][which.min(RMSE[Type == "Combinaison"])],
    
    .groups = "drop"
  ) |>
  mutate(
    # Calcul du gain/perte 
    Gain_Pct = ( Best_Single_RMSE - Best_Combo_RMSE ) / Best_Combo_RMSE,
  )

# Graphique
ggplot(df_synergy, aes(y = Scenario)) +
  geom_segment(aes(x = Best_Single_RMSE, xend = Best_Combo_RMSE, y = Scenario, yend = Scenario), color = "grey70") +
  
  # Point Rouge : Meilleur Modèle Seul
  geom_point(aes(x = Best_Single_RMSE, color = "Meilleur Modèle Seul"), size = 3.5) +
  
  # Point Vert : Meilleure Combinaison
  geom_point(aes(x = Best_Combo_RMSE, color = "Meilleure Combinaison"), size = 3.5) +
  
  # Séparation par Mois
  facet_wrap(~Mois, scales = "free_x") +
  
  #Légende
  scale_color_manual(values = c("Meilleur Modèle Seul" = "#E74C3C", "Meilleure Combinaison" = "#27AE60")) +
  
  labs(
    title = "Apport de la Combinaison vs Meilleur Modèle Individuel",
    subtitle = "Si le point Vert est à gauche du Rouge = La combinaison réduit l'erreur (Succès)",
    x = "RMSE (Plus bas est mieux)",
    y = "",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold")
  )

# df calcul de gains
print("--- GAINS DE PERFORMANCE PAR SCENARIO ---")
print(df_synergy |> 
        select(Scenario, Mois, Name_Best_Single, Name_Best_Combo, Gain_Pct) |> 
        mutate(Gain_Pct = percent(Gain_Pct, accuracy = 0.01)), n =21)