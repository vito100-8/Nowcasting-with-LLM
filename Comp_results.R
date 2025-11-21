########## COMPARAISON DES RESULTATS ENTRE LES DIFFERENTS MODELES ####################

rm(list = ls()) 
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")


#A FAIRE : ADAPTER A LA STRUCTURE MONTHLY LES DONNEES, REVERIFIER OUTPUT

#######################
#INPUTS
###########################

# Définissez les chemins des fichiers de résultats BDF et INSEE pour le même modèle
MODEL_NAME <- "No Text" # Nom du modèle pour référence
FILE_PATH_BDF <- "Final_results/BDF_noText_2020.xlsx" 
FILE_PATH_INSEE <- "Final_results/INSEE_noText_2020.xlsx" 


# Préparation des données

message(paste("Analyse comparative pour le modèle:", MODEL_NAME))

wide_BDF <- read_xlsx(FILE_PATH_BDF) |> mutate(Date = as.Date(Date))
wide_INSEE <- read_xlsx(FILE_PATH_INSEE) |> mutate(Date = as.Date(Date))

# Conversion au format long
bdf_long <- to_long(wide_BDF, "BDF")
insee_long <- to_long(wide_INSEE, "INSEE")
both_long <- bind_rows(bdf_long, insee_long)

####################################
# Stats descriptives
####################################

#Stats 
stats_des <- both_long |>
  group_by(Date, source) |>
  summarise(
    Moyenne = mean(forecast, na.rm = TRUE),
    Médiane = median(forecast, na.rm = TRUE),
    Variance = var(forecast, na.rm = TRUE),
    EcartType = sd(forecast, na.rm = TRUE),
    Skewness = skewness(forecast, na.rm = TRUE),
    Kurtosis = kurtosis(forecast, na.rm = TRUE),
    Moyenne_Confiance = mean(confidence, na.rm = TRUE),
    Observations = n(),
    .groups = "drop"
  )




# Corrélation et T-Test sur les Moyennes
df_BDF_means <- wide_BDF |> 
  rowwise() |>
  mutate(BDF_median = mean(c_across( starts_with("forecast_")), na.rm = TRUE)) |>
  select(Date, BDF_mean)

df_INSEE_means <- wide_INSEE |> 
  rowwise() |>
  mutate(INSEE_median = mean(c_across( starts_with("forecast_")), na.rm = TRUE)) |>
  select(Date, INSEE_mean)

df_cor_means <- inner_join(df_BDF_means, df_INSEE_means, by = "Date")

# Corrélation Spearman entre les moyennes
correlation_mean <- cor(df_cor_means$BDF_mean, df_cor_means$INSEE_mean, method = "spearman", use = "pairwise.complete.obs")


# T-Test sur les moyennes (variance inégale)
t_test_results <- tryCatch({
  t.test(df_cor_means$BDF_mean, df_cor_means$INSEE_mean, var.equal = FALSE)
}, error = function(e) {
  message("Erreur lors du t-test (probablement trop peu de données).")
  return(NULL)
})

#############################################
# Représentations graphique
##########################################


#  Boxplot 
plot_boxplot <- ggplot(both_long, aes(x = source, y = as.numeric(forecast), fill = source)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1.5) +
  # Une facette par date
  facet_wrap(~ Date, scales = "free_y") + 
  labs(
    title = paste("Boxplots de Prévisions (", MODEL_NAME, ") : BDF vs INSEE"),
    y = "Prévision",
    x = "Organisme",
    fill = "Source"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_boxplot)

# Distribution
plot_density <- ggplot(both_long, aes(x = as.numeric(forecast), fill = source, color = source)) +
  geom_density(alpha = 0.4) +
  # Une facette par date
  facet_wrap(~ Date, scales = "free") + 
  labs(
    title = paste("Distributions de Prévisions (", MODEL_NAME, ") : BDF vs INSEE"),
    x = "Prévision",
    y = "Densité",
    fill = "Source"
  ) +
  theme_minimal()

print(plot_density)
