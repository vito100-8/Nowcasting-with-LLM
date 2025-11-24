########## COMPARAISON DES RESULTATS ENTRE LES DIFFERENTS MODELES ####################

rm(list = ls()) 
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")


#A FAIRE : ADAPTER A LA STRUCTURE MONTHLY LES DONNEES, REVERIFIER OUTPUT

#######################
#INPUTS
###########################

# chemins des fichiers de résultats BDF et INSEE pour le même modèle
MODEL_NAME <- "ECO Text" # Nom du modèle pour référence
FILE_PATH_BDF <- "Final_results/BDF_ECO_text_2020.xlsx" 
FILE_PATH_INSEE <- "Final_results/INSEE_ECO_text_2020.xlsx" 



# Préparation des données

message(paste("Analyse comparative pour le modèle:", MODEL_NAME))

stats_BDF <- read_xlsx(FILE_PATH_BDF) |>
  rowwise() |>
  mutate(Date = Date,
         median_forecast = median(c_across(starts_with("forecast_")), na.rm = TRUE),
         median_conf = median(c_across(starts_with("confidence_")), na.rm = TRUE),
         source = "BDF",
         .keep = "none")
stats_INSEE <- read_xlsx(FILE_PATH_INSEE) |> 
  rowwise() |>
  mutate(Date = Date,
         median_forecast = median(c_across(starts_with("forecast_")), na.rm = TRUE),
         median_conf = median(c_across(starts_with("confidence_")), na.rm = TRUE),
         source = "INSEE",
          .keep = "none")



#bind
stats_both <- bind_rows(stats_BDF, stats_INSEE)

#mensualisation des données
stats_both <- stats_both |>
  mutate(
    month = month(Date),
    year = year(Date),
    forecast_quarter = case_when(
      month %in% c(2, 3, 4) ~ 1, month %in% c(5, 6, 7) ~ 2,
      month %in% c(8, 9, 10) ~ 3, month %in% c(11, 12, 1) ~ 4
    ),
    month_in_quarter = case_when( 
      month %in% c(2, 5, 8, 11) ~ 1, # M1 
      month %in% c(3, 6, 9, 12) ~ 2, # M2 
      month %in% c(4, 7, 10, 1) ~ 3  # M3 
    ),
    Mois_Label = paste0("Mois ", month_in_quarter),
    forecast_year = case_when(month == 1 ~ year - 1, TRUE ~ year)
  )


####################################
# Stats descriptives
####################################

#Stats 
stats_des <- stats_both |>
  group_by(Mois_Label, source) |>
  summarise(
    Moyenne = mean(median_forecast, na.rm = TRUE),
    Médiane = median(median_forecast, na.rm = TRUE),
    Variance = var(median_forecast, na.rm = TRUE),
    EcartType = sd(median_forecast, na.rm = TRUE),
    Skewness = skewness(median_forecast, na.rm = TRUE),
    Kurtosis = kurtosis(median_forecast, na.rm = TRUE),
    Moyenne_Confiance = mean(median_conf, na.rm = TRUE),
    Observations = n(),
    .groups = "drop"
  )


# Corrélation et T-Test sur les Moyennes
df_BDF_means <- stats_BDF |> 
  mutate(BDF_mean = mean(median_forecast), na.rm = TRUE) |>
  select(Date, BDF_mean)

df_INSEE_means <- stats_INSEE |> 
  mutate(INSEE_mean = mean(median_forecast), na.rm = TRUE) |>
  select(Date, INSEE_mean)

df_cor_means <- inner_join(df_BDF_means, df_INSEE_means, by = "Date")

# Corrélation Spearman entre les moyennes
correlation_mean <- cor(df_cor_means$BDF_mean, df_cor_means$INSEE_mean, method = "spearman", use = "pairwise.complete.obs")


# T-Test sur les moyennes (variance inégale)
t_test_results <- t.test(df_cor_means$BDF_mean, df_cor_means$INSEE_mean, var.equal = FALSE)


#############################################
# Représentations graphique
##########################################


#  Boxplot 
boxplot <- ggplot(stats_both, aes(x = source, y = as.numeric(median_forecast), fill = source)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1) + 
  # facet par mois
  facet_wrap(~ Mois_Label, scales = "free_y") + 
  labs(
    title = paste("Boxplots de Prévisions (", MODEL_NAME, ") : BDF vs INSEE"),
    y = "Prévision",
    x = "Institution",
    fill = "Source"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(-2, 2))

print(boxplot)

#distribution des prévisions
density_prev <- ggplot(stats_both, aes(x = as.numeric(median_forecast), fill = source)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ Mois_Label, scales = "free") + 
  labs(
    title = paste("Distributions de Prévisions (", MODEL_NAME, ") : BDF vs INSEE"),
    x = "Prévision",
    y = "Densité",
    fill = "Source"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(-1, 1.5))

print(density_prev)

#boxplot des erreurs

##Nettoyage du PIB 
df_PIB <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")


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

err_join <- left_join(stats_both, pib, by = c("forecast_year", "forecast_quarter")) 
err_join <- err_join |> 
  select(!Date.y) |>
  mutate(Date = Date.x,
         errors = PIB_PR - median_forecast,
         .keep = "unused")

boxplot_err <- ggplot(err_join, aes(x = source, y = errors, fill = source)) +
  geom_boxplot(alpha = 0.6, outlier.colour = "red", outlier.shape = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  facet_wrap(~ Mois_Label, scales = "fixed") + 
  labs(
    title = paste("Distribution des erreurs de prévision (", MODEL_NAME, ")"),
    subtitle = "La ligne pointillée indique une erreur nulle",
    x = "Institution",
    y = "Erreur de prévision (Points de PIB)",
    fill = "Source"
  ) +
  theme_minimal()

print(boxplot_err)
  
#série temporelle des erreurs
line_err <- ggplot(err_join, aes(x = as.Date(Date), y = errors, color = source, group = source)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  facet_wrap(~ Mois_Label, ncol = 1, scales = "fixed") + 
  scale_x_date(
    date_breaks = 'year',  
    date_labels = "%Y",      
    limits = as.Date(c("2015-01-01", "2025-12-31")) 
  ) +
  labs(
    title = paste("Évolution des erreurs dans le temps (", MODEL_NAME, ")"),
    x = "Date",
    y = "Erreur",
    color = "Source"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") 

print(line_err)

