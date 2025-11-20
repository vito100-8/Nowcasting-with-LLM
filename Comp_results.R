########## COMPARAISON DES RESULTATS ENTRE LES DIFFERENTS MODELES ####################

rm(list = ls()) 

##########
#ATTENTION: n'exécuter que les "source" nécessaires car sinon lance toutes les boucles de chaque fichier
###########
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")
source("LLM_AR.R")
source("LLM_Text.R")
source("LLM_noText.R")
source("LLM_just_text.R")
source("LLM_excel.R")
source("LLM_excel_with_error.R")
source("LLM_rolling_text.R")
source("LLM_Text_12.R")
source("LLM_all_inputs.R")
source("Modèle_ISMA.R")

############################
# Telechargement du PIB réel
###########################

pib_reel <- read_xlsx("Data_PIB_ENQ_2.xlsx", sheet = "data_Q")
pib_reel <- pib_reel |>
  select(dates:PIB_PR)


##################
#Stats Descriptives
###################

#wide_BDF <- df_xxxx
#wide_insee <- df_xxxx   Choisir selon le noms des résultats

bdf_long   <- to_long(wide_BDF, "BDF")
insee_long <- to_long(wide_insee, "INSEE")

both_long <- bind_rows(bdf_long, insee_long)


# Stats descriptives simples
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
    .groups = "drop"
  )

#Corrélation entre les prévisions

df_BDF   <- wide_bdf |> select(Date, starts_with("forecast_"))
df_INSEE <- wide_insee |> select(Date, starts_with("forecast_"))

colnames(df_BDF)[-1]   <- paste0("BDF_",   seq_along(colnames(df_BDF)[-1]))
colnames(df_INSEE)[-1] <- paste0("INSEE_", seq_along(colnames(df_INSEE)[-1]))

df_BDF <- df_BDF |> 
  select(!Date)

df_INSEE <- df_INSEE |> 
  select(!Date)


BDF_vec   <- rowMeans(df_BDF, na.rm = TRUE)
INSEE_vec <- rowMeans(df_INSEE, na.rm = TRUE)


correlation <- cor(BDF_vec, INSEE_vec, method = "spearman") 

#correlation within date

df_BDF_long <- wide_bdf |>
  pivot_longer(cols = starts_with("forecast_"), names_to = "repro", values_to = "BDF_forecast") |>
  select("Date", "repro","BDF_forecast")

df_INSEE_long <- wide_insee |>
  pivot_longer(cols = starts_with("forecast_"), names_to = "repro", values_to = "INSEE_forecast") |>
  select("Date", "repro","INSEE_forecast")

df_merged <- df_BDF_long |>
  inner_join(df_INSEE_long, by = c("Date", "repro"))

cor(df_merged$BDF_forecast, df_merged$INSEE_forecast, method = "spearman") #corr =~ 0.64 obtenue


#Test de moyenne entre BDF et INSEE
t.test_BDF <- df_BDF |>
  select(!Date)
t.test_INSEE <- df_INSEE |>
  select(!Date)

t.test(t.test_BDF, t.test_INSEE, var.equal = FALSE) 
# En supposant d'après les résultats précédent (mais à confirmer 
# avec un plus gros échantillon) que la variances est différente entre les deux



################
#GRAPHIQUES
################

#Distribution  des prev selon BDF/INSEE pour chaque date : violin (((à voir lequel plus pertinent)))
ggplot(both_long, aes(x = source, y = as.numeric(forecast), fill = source)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  facet_wrap(~ Date, scales = "free_y") +
  labs(
    title = "Distribution des prévisions par organisme",
    y = "Prévision",
    x = "Organisme"
  ) +
  theme_minimal()

# Boxplot
ggplot(both_long, aes(x = source, y = as.numeric(forecast), fill = source)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1.5) +
  facet_wrap(~ Date, scales = "free_y") +
  labs(
    title = "Distribution des prévisions par organisme (Boxplot)",
    y = "Prévision",
    x = "Organisme"
  ) +
  theme_minimal()

#Densité
ggplot(both_long, aes(x = as.numeric(forecast), fill = source, color = source)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ Date, scales = "free") +
  labs(
    title = "Distribution des prévisions BDF vs INSEE",
    x = "Prévision",
    y = "Densité"
  ) +
  theme_minimal()

