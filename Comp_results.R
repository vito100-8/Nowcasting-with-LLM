########## COMPARAISON DES RESULTATS ENTRE LES DIFFERENTS MODELES ####################

rm(list = ls())
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Cutoff_perf_analysis.R")
source("LLM_MAE_RMSE_function.R")

###########################
# INPUTS
###########################

# chemins des fichiers de résultats BDF et INSEE pour le même modèle
MODEL_NAME <- " Text" # Nom du modèle pour référence
FILE_PATH_BDF <- "Final_results/BDF_text_2020.xlsx"
FILE_PATH_INSEE <- "Final_results/INSEE_text_2020.xlsx"


# Préparation des données

message(paste("Analyse comparative pour le modèle:", MODEL_NAME))

stats_BDF <- read_xlsx(FILE_PATH_BDF) |>
  rowwise() |>
  mutate(
    Date = Date,
    median_forecast = median(c_across(starts_with("forecast_")), na.rm = TRUE),
    median_conf = median(c_across(starts_with("confidence_")), na.rm = TRUE),
    source = "BDF",
    .keep = "none"
  )
stats_INSEE <- read_xlsx(FILE_PATH_INSEE) |>
  rowwise() |>
  mutate(
    Date = Date,
    median_forecast = median(c_across(starts_with("forecast_")), na.rm = TRUE),
    median_conf = median(c_across(starts_with("confidence_")), na.rm = TRUE),
    source = "INSEE",
    .keep = "none"
  )


# bind
stats_both <- bind_rows(stats_BDF, stats_INSEE)

# mensualisation des données
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
      month %in% c(4, 7, 10, 1) ~ 3 # M3
    ),
    Mois_Label = paste0("Mois ", month_in_quarter),
    forecast_year = case_when(month == 1 ~ year - 1, TRUE ~ year)
  )

stats_both <- stats_both |>
  mutate(Mois_Label = str_replace(Mois_Label, "Mois ", "M"))

# Chargement PIB pour calculer les erreurs

## Nettoyage du PIB
df_PIB <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")


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

err_join <- left_join(stats_both, pib, by = c("forecast_year", "forecast_quarter"))
err_join <- err_join |>
  select(!Date.y) |>
  mutate(
    Date = Date.x,
    errors = PIB_PR - median_forecast,
    .keep = "unused"
  )


err_join_comp <- left_join(stats_both, pib, by = c("forecast_year", "forecast_quarter"))

####################################
# Stats descriptives
####################################

# Stats
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
test_BDF <- stats_BDF |>
  select(Date, median_forecast)

test_insee <- stats_INSEE |>
  select(Date, median_forecast)

test_both <- inner_join(test_BDF, test_insee, by = "Date")
# Corrélation Spearman
correlation_mean <- cor(test_both$median_forecast.x, test_both$median_forecast.y, method = "spearman", use = "pairwise.complete.obs")


# T-Test sur les moyennes (variance inégale)
t_test_results <- t.test(test_both$median_forecast.x, test_both$median_forecast.y, var.equal = FALSE)


#############################################
# Représentations graphique
##########################################


#  Boxplot
boxplot <- ggplot(stats_both, aes(x = source, y = as.numeric(median_forecast), fill = source)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1) +
  # facet par mois
  facet_wrap(~Mois_Label, scales = "free_y") +
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

# distribution des prévisions
density_prev <- ggplot(stats_both, aes(x = as.numeric(median_forecast), fill = source)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Mois_Label, scales = "free") +
  labs(
    title = paste("Distributions de Prévisions (", MODEL_NAME, ") : BDF vs INSEE"),
    x = "Prévision",
    y = "Densité",
    fill = "Source"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(-1, 1.5))

print(density_prev)

# boxplot des erreurs


boxplot_err <- ggplot(err_join, aes(x = source, y = errors, fill = source)) +
  geom_boxplot(alpha = 0.6, outlier.colour = "red", outlier.shape = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~Mois_Label, scales = "fixed") +
  labs(
    title = paste("Distribution des erreurs de prévision (", MODEL_NAME, ")"),
    subtitle = "La ligne pointillée indique une erreur nulle",
    x = "Institution",
    y = "Erreur de prévision (Points de PIB)",
    fill = "Source"
  ) +
  theme_minimal()

print(boxplot_err)

err_join_nocovid <- err_join |>
  filter(dates < "2020-02-01" | dates > "2022-01-01")
# densité des erreurs
density_error_prev <- ggplot(err_join_nocovid, aes(x = errors, fill = source)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Mois_Label, scales = "free") +
  labs(
    title = paste("Distribution of errors"),
    subtitle = ("BDF_txt vs Insee_txt"),
    x = "Error",
    y = "Density",
    fill = "Source"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(-1, 1.5))

print(density_error_prev)


# série temporelle des erreurs
line_err <- ggplot(err_join, aes(x = as.Date(Date), y = errors, color = source, group = source)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  facet_wrap(~Mois_Label, ncol = 1, scales = "fixed") +
  scale_x_date(
    date_breaks = "year",
    date_labels = "%Y",
    limits = as.Date(c("2015-01-01", "2019-12-31"))
  ) +
  labs(
    title = paste("Évolution des erreurs dans le temps (", MODEL_NAME, ")"),
    x = "Date",
    y = "Erreur",
    color = "Source"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(-0.5, 0.5))

print(line_err)


# Distribution niveau de confiance BDF vs INSEE

density_conf <- ggplot(stats_both, aes(x = median_conf, fill = source)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Mois_Label, scales = "free") +
  labs(
    title = paste("Distribution of confidence level"),
    subtitle = ("BDF_txt vs Insee_txt with the COVID period"),
    x = "Confidence level",
    y = "Density",
    fill = "Source"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 100))


print(density_conf)

stats_both_nocovid <- stats_both |>
  filter(!(Date >= "2020-02-01" & Date < "2022-02-01"))

density_conf_nocovid <- ggplot(stats_both_nocovid, aes(x = median_conf, fill = source)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Mois_Label, scales = "free") +
  labs(
    title = paste("Distribution of confidence level"),
    subtitle = ("BDF_txt vs Insee_txt without the COVID period"),
    x = "Confidence level",
    y = "Density",
    fill = "Source"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 100))

print(density_conf_nocovid)


#################################################################################
# Comparaison des RMSE selon PIB ou PIB_PR
#################################################################################

# (à lancer après déclaration des variables ci-dessus)
err_join_comp <- err_join_comp |>
  mutate(
    Date = Date.x,
    errors_PIB_PR = PIB_PR - median_forecast,
    errors_PIB = PIB - median_forecast,
    .keep = "unused"
  )

results_diff <- err_join_comp |>
  group_by(Mois_Label, source) |>
  summarise(
    # Stats PIB_PR
    MAE_PR = mean(abs(errors_PIB_PR), na.rm = TRUE),
    RMSE_PR = sqrt(mean(errors_PIB_PR^2, na.rm = TRUE)),

    # Stats PIB
    MAE_Rev = mean(abs(errors_PIB), na.rm = TRUE),
    RMSE_Rev = sqrt(mean(errors_PIB^2, na.rm = TRUE)),

    # Comp ( - -> erreur plus faible pour PIB - prev)
    Diff_RMSE = RMSE_Rev - RMSE_PR,
    .groups = "drop"
  )


################################################################################
# MEME ANALYSE QUE Comp_results_function.R (mais ici sans fonction pour
# l'analyse des RMSE)


################################################################################
# ANALYSE COMPARATIVE DES MODELES
##################################################################################

# Rang des modèles
df_ranked <- final_period_monthly_analysis |>
  mutate(across(c(starts_with("MAE"), starts_with("RMSE")), rank)) |>
  pivot_longer(
    cols = -Model,
    names_to = "Metrique",
    values_to = "Rang"
  )


ggplot(df_ranked, aes(x = Metrique, y = Model, fill = Rang)) +
  geom_tile(color = "white", lwd = 1) +
  geom_text(aes(label = Rang), color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#2E86C1", high = "#D6EAF8") +
  theme_minimal() +
  labs(
    title = "Classement des modèles",
    x = "", y = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Classement de modèles d'après le rang moyen

df_rank_mean <- df_ranked |>
  group_by(Model) |>
  summarise(
    # Moyenne des rangs basée sur le MAE
    Mean_Rank_MAE = mean(
      Rang[str_detect(Metrique, "^MAE_")],
      na.rm = TRUE
    ),

    # Moyenne des rangs basée sur le RMSE
    Mean_Rank_RMSE = mean(
      Rang[str_detect(Metrique, "^RMSE_")],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(Mean_Rank_MAE)


################################################################################
# Classement meilleurs modèle (RMSE)
#################################################################################
## Période entière
###################

df_eco <- read_xlsx("./Results_Eco_Climat.xlsx") |>
  filter(dates > "2015-01-01")

df_eco_bdf <- df_eco |>
  select(PIB_PR, BDF_ALL_M1:BDF_ALL_M3)


df_eco_insee <- df_eco |>
  select(PIB_PR, INSEE_ALL_M1:INSEE_ALL_M3)

df_eco_all <- df_eco |>
  select(PIB_PR, COMB_ALL_M1:COMB_ALL_M3)


# CALCUL RMSE DES MODELES ET AGREGATION

rmse_BDF <- df_eco_bdf |>
  na.omit() |>
  summarise(
    Model = "ECO_BDF",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - BDF_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - BDF_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - BDF_ALL_M3)^2))
  )

rmse_INSEE <- df_eco_insee |>
  na.omit() |>
  summarise(
    Model = "ECO_INSEE",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - INSEE_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - INSEE_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - INSEE_ALL_M3)^2))
  )

rmse_COMB <- df_eco_all |>
  na.omit() |>
  summarise(
    Model = "ECO_ALL",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - COMB_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - COMB_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - COMB_ALL_M3)^2))
  )

rmse_table <- bind_rows(rmse_BDF, rmse_INSEE, rmse_COMB)

rmse_table <- rmse_table |>
  rename_with(
    .fn = ~ gsub("RMSE_Mois_", "RMSE_M", .),
    .cols = starts_with("RMSE_Mois_")
  )

metrics_recap_final_2 <- metrics_recap_final |>
  select(Model, starts_with("RMSE")) |>
  rename_with(
    .fn = ~ gsub("RMSE_Mois_", "RMSE_M", .),
    .cols = starts_with("RMSE_Mois_")
  )

metrics_final <- bind_rows(rmse_table, metrics_recap_final_2)


df_metrics_all <- metrics_final |>
  filter(Model %in% c("BDF_txt", "INSEE_txt", "ALL_txt", "all_txt", "ECO_ALL", "ECO_BDF", "ECO_INSEE")) |>
  pivot_longer(
    cols = starts_with("RMSE"),
    names_to = "Mois",
    values_to = "RMSE"
  ) |>
  mutate(
    Model = ifelse(Model == "all_txt", "ALL_txt", Model),
    Type = case_when(
      str_detect(Model, "^BDF") ~ "LLM BDF",
      str_detect(Model, "^INSEE") ~ "LLM INSEE",
      str_detect(Model, "^ECO") ~ "ECONOMETRICS",
      TRUE ~ "LLM ALL"
    )
  )

df_metrics_all <- df_metrics_all |>
  mutate(Mois = str_replace(Mois, "RMSE_", ""))

df_col_top <- df_metrics_all |>
  group_by(Mois) |>
  mutate(Model_Ordered = reorder_within(Model, -RMSE, Mois)) |>
  ungroup()

custom_colors <- c(
  "LLM BDF" = "cornflowerblue",
  "LLM INSEE" = "cyan4",
  "LLM ALL" = "cyan",
  "ECONOMETRICS" = "coral1"
)

ggplot(df_col_top, aes(x = RMSE, y = Model_Ordered, fill = Type)) +
  geom_col(width = 0.7, alpha = 0.9) +

  # Affichage de la valeur exacte au bout de la barre
  geom_text(aes(label = round(RMSE, 3)), hjust = -0.2, size = 3.5, fontface = "bold") +

  # Un facet par mois
  facet_wrap(~Mois, scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_manual(
    values = custom_colors
  ) +

  # Légendes
  labs(
    title = "Models ranking based on RMSE",
    subtitle = "2015-2025",
    x = "RMSE",
    y = "",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "#2C3E50", color = NA),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  ) +
  # pour voir le texte aux marges
  expand_limits(x = max(df_col_top$RMSE) * 1.15)

###################
## Sous-périodes :
###################


df_eco <- read_xlsx("./Results_Eco_Climat.xlsx") |>
  filter(dates > "2015-01-01")

# 2015-2019 :
df_eco <- df_eco |>
  filter(dates < "2020-02-01")

df_eco_bdf <- df_eco |>
  select(PIB_PR, BDF_ALL_M1:BDF_ALL_M3)


df_eco_insee <- df_eco |>
  select(PIB_PR, INSEE_ALL_M1:INSEE_ALL_M3)

df_eco_all <- df_eco |>
  select(PIB_PR, COMB_ALL_M1:COMB_ALL_M3)


# CALCUL RMSE DES MODELES ET AGREGATION

rmse_BDF <- df_eco_bdf |>
  na.omit() |>
  summarise(
    Model = "ECO_BDF",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - BDF_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - BDF_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - BDF_ALL_M3)^2))
  )

rmse_INSEE <- df_eco_insee |>
  na.omit() |>
  summarise(
    Model = "ECO_INSEE",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - INSEE_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - INSEE_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - INSEE_ALL_M3)^2))
  )

rmse_COMB <- df_eco_all |>
  na.omit() |>
  summarise(
    Model = "ECO_ALL",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - COMB_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - COMB_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - COMB_ALL_M3)^2))
  )

rmse_table <- bind_rows(rmse_BDF, rmse_INSEE, rmse_COMB)

rmse_table <- rmse_table |>
  rename_with(
    .fn = ~ gsub("RMSE_Mois_", "RMSE_M", .),
    .cols = starts_with("RMSE_Mois_")
  )

# df subperiod RMSE
final_period_monthly_analysis <- read_xlsx("Analysis_monthy_period.xlsx")


metrics_recap_final_2 <- final_period_monthly_analysis |>
  select(Model, starts_with("RMSE_2015")) |>
  rename_with(
    .fn = ~ gsub("RMSE_2015_2019_M", "RMSE_M", .),
    .cols = starts_with("RMSE_2015_2019_M")
  )

metrics_final <- bind_rows(rmse_table, metrics_recap_final_2)


df_metrics_all <- metrics_final |>
  filter(Model %in% c("BDF_txt", "INSEE_txt", "ECO_ALL", "ECO_BDF", "ECO_INSEE", "All_txt", "all_txt")) |>
  pivot_longer(
    cols = starts_with("RMSE"),
    names_to = "Mois",
    values_to = "RMSE"
  ) |>
  mutate(
    Model = ifelse(Model == "all_txt", "ALL_txt", Model),
    Type = case_when(
      str_detect(Model, "^BDF") ~ "LLM BDF",
      str_detect(Model, "^INSEE") ~ "LLM INSEE",
      str_detect(Model, "^ECO") ~ "ECONOMETRICS",
      TRUE ~ "LLM ALL"
    )
  )
df_metrics_all <- df_metrics_all |>
  mutate(Mois = str_replace(Mois, "RMSE_", ""))

df_col_top <- df_metrics_all |>
  group_by(Mois) |>
  mutate(Model_Ordered = reorder_within(Model, -RMSE, Mois)) |>
  ungroup()

custom_colors <- c(
  "LLM BDF" = "cornflowerblue",
  "LLM INSEE" = "cyan4",
  "LLM ALL" = "cyan",
  "ECONOMETRICS" = "coral1"
)

ggplot(df_col_top, aes(x = RMSE, y = Model_Ordered, fill = Type)) +
  geom_col(width = 0.7, alpha = 0.9) +

  # Affichage de la valeur exacte au bout de la barre
  geom_text(aes(label = round(RMSE, 3)), hjust = -0.2, size = 3.5, fontface = "bold") +

  # Un facet par mois
  facet_wrap(~Mois, scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_manual(
    values = custom_colors
  ) +


  # Légendes
  labs(
    title = "Models ranking based on RMSE",
    subtitle = "2015 - 2019",
    x = "RMSE",
    y = "",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "#2C3E50", color = NA),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  ) +
  # pour voir le texte aux marges
  expand_limits(x = max(df_col_top$RMSE) * 1.15)


# 2020-2025 :

df_eco <- read_xlsx("./Results_Eco_Climat.xlsx") |>
  filter(dates >= "2020-02-01")


df_eco_bdf <- df_eco |>
  select(PIB_PR, BDF_ALL_M1:BDF_ALL_M3)


df_eco_insee <- df_eco |>
  select(PIB_PR, INSEE_ALL_M1:INSEE_ALL_M3)

df_eco_all <- df_eco |>
  select(PIB_PR, COMB_ALL_M1:COMB_ALL_M3)


# CALCUL RMSE DES MODELES ET AGREGATION


rmse_BDF <- df_eco_bdf |>
  na.omit() |>
  summarise(
    Model = "ECO_BDF",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - BDF_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - BDF_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - BDF_ALL_M3)^2))
  )

rmse_INSEE <- df_eco_insee |>
  na.omit() |>
  summarise(
    Model = "ECO_INSEE",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - INSEE_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - INSEE_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - INSEE_ALL_M3)^2))
  )

rmse_COMB <- df_eco_all |>
  na.omit() |>
  summarise(
    Model = "ECO_ALL",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - COMB_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - COMB_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - COMB_ALL_M3)^2))
  )

rmse_table <- bind_rows(rmse_BDF, rmse_INSEE, rmse_COMB)

rmse_table <- rmse_table |>
  rename_with(
    .fn = ~ gsub("RMSE_Mois_", "RMSE_M", .),
    .cols = starts_with("RMSE_Mois_")
  )

metrics_recap_final_2 <- final_period_monthly_analysis |>
  select(Model, starts_with("RMSE_2020")) |>
  rename_with(
    .fn = ~ gsub("RMSE_2020_2025_M", "RMSE_M", .),
    .cols = starts_with("RMSE_2020_2025_M")
  )

metrics_final <- bind_rows(rmse_table, metrics_recap_final_2)


df_metrics_all <- metrics_final |>
  filter(Model %in% c("BDF_txt", "INSEE_txt", "ALL_txt", "all_txt", "ECO_ALL", "ECO_BDF", "ECO_INSEE")) |>
  pivot_longer(
    cols = starts_with("RMSE"),
    names_to = "Mois",
    values_to = "RMSE"
  ) |>
  mutate(
    Model = ifelse(Model == "all_txt", "ALL_txt", Model),
    Type = case_when(
      str_detect(Model, "^BDF") ~ "LLM BDF",
      str_detect(Model, "^INSEE") ~ "LLM INSEE",
      str_detect(Model, "^ECO") ~ "ECONOMETRICS",
      TRUE ~ "LLM ALL"
    )
  )

df_metrics_all <- df_metrics_all |>
  mutate(Mois = str_replace(Mois, "RMSE_", ""))

df_col_top <- df_metrics_all |>
  group_by(Mois) |>
  mutate(Model_Ordered = reorder_within(Model, -RMSE, Mois)) |>
  ungroup()

custom_colors <- c(
  "LLM BDF" = "cornflowerblue",
  "LLM INSEE" = "cyan4",
  "LLM ALL" = "cyan",
  "ECONOMETRICS" = "coral1"
)


ggplot(df_col_top, aes(x = RMSE, y = Model_Ordered, fill = Type)) +
  geom_col(width = 0.7, alpha = 0.9) +

  # Affichage de la valeur exacte au bout de la barre
  geom_text(aes(label = round(RMSE, 3)), hjust = -0.2, size = 3.5, fontface = "bold") +

  # Un facet par mois
  facet_wrap(~Mois, scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_manual(
    values = custom_colors
  ) +


  # Légendes
  labs(
    title = "Models ranking based on RMSE",
    subtitle = "2020 - 2025",
    x = "RMSE",
    y = "",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "#2C3E50", color = NA),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  ) +
  # pour voir le texte aux marges
  expand_limits(x = max(df_col_top$RMSE) * 1.15)

##############################################
# Graph sur toutes la période + tous les modèles
###############################################

df_eco <- read_xlsx("./Results_Eco_Climat.xlsx") |>
  filter(dates > "2015-01-01")

df_AR_2 <- df_eco |>
  select(PIB_PR, AR_M1:AR_M3)

df_eco_bdf <- df_eco |>
  select(PIB_PR, BDF_ALL_M1:BDF_ALL_M3)


df_eco_insee <- df_eco |>
  select(PIB_PR, INSEE_ALL_M1:INSEE_ALL_M3)

df_eco_all <- df_eco |>
  select(PIB_PR, COMB_ALL_M1:COMB_ALL_M3)

df_eco_bdf_ind <- df_eco |>
  select(PIB_PR, BDF_IND_M1:BDF_IND_M3)

df_eco_insee_ind <- df_eco |>
  select(PIB_PR, INSEE_IND_M1:INSEE_IND_M3)


# CALCUL RMSE DES MODELES ET AGREGATION
rmse_AR_2 <- df_AR_2 |>
  na.omit() |>
  summarise(
    Model = "ECO_AR",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - AR_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - AR_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - AR_M3)^2))
  )


rmse_BDF <- df_eco_bdf |>
  na.omit() |>
  summarise(
    Model = "ECO_BDF",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - BDF_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - BDF_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - BDF_ALL_M3)^2))
  )

rmse_INSEE <- df_eco_insee |>
  na.omit() |>
  summarise(
    Model = "ECO_INSEE",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - INSEE_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - INSEE_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - INSEE_ALL_M3)^2))
  )

rmse_COMB <- df_eco_all |>
  na.omit() |>
  summarise(
    Model = "ECO_ALL",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - COMB_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - COMB_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - COMB_ALL_M3)^2))
  )

rmse_BDF_ind <- df_eco_bdf_ind |>
  na.omit() |>
  summarise(
    Model = "ECO_BDF_IND",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - BDF_IND_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - BDF_IND_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - BDF_IND_M3)^2))
  )

rmse_Insee_ind <- df_eco_insee_ind |>
  na.omit() |>
  summarise(
    Model = "ECO_INSEE_IND",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - INSEE_IND_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - INSEE_IND_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - INSEE_IND_M3)^2))
  )


rmse_table <- bind_rows(rmse_AR_2, rmse_BDF, rmse_INSEE, rmse_COMB, rmse_BDF_ind, rmse_Insee_ind)


metrics_recap_final_2 <- metrics_recap_final |>
  select(Model, starts_with("RMSE"))

metrics_final <- bind_rows(rmse_table, metrics_recap_final_2)

metrics_final <- metrics_final |>
  rename_with(
    .fn = ~ gsub("RMSE_Mois_", "RMSE_M", .),
    .cols = starts_with("RMSE_Mois_")
  )

df_metrics_all <- metrics_final |>
  pivot_longer(
    cols = starts_with("RMSE"),
    names_to = "Mois",
    values_to = "RMSE"
  ) |>
  mutate(
    Model = ifelse(Model == "all_txt", "ALL_txt", Model),
    Type = case_when(
      str_detect(Model, "^BDF") ~ "LLM BDF",
      str_detect(Model, "^INSEE") ~ "LLM INSEE",
      str_detect(Model, "^ECO") ~ "ECONOMETRICS",
      TRUE ~ "LLM ALL"
    )
  )

df_metrics_all <- df_metrics_all |>
  mutate(Mois = str_replace(Mois, "RMSE_", ""))

df_col_top <- df_metrics_all |>
  group_by(Mois) |>
  mutate(Model_Ordered = reorder_within(Model, -RMSE, Mois)) |>
  ungroup()

custom_colors <- c(
  "LLM BDF" = "cornflowerblue",
  "LLM INSEE" = "cyan4",
  "LLM ALL" = "cyan",
  "ECONOMETRICS" = "coral1"
)


ggplot(df_col_top, aes(x = RMSE, y = Model_Ordered, fill = Type)) +
  geom_col(width = 0.7, alpha = 0.9) +

  # Affichage de la valeur exacte au bout de la barre
  geom_text(aes(label = round(RMSE, 3)), hjust = -0.2, size = 3.5, fontface = "bold") +

  # Un facet par mois
  facet_wrap(~Mois, scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_manual(
    values = custom_colors
  ) +


  # Légendes
  labs(
    title = "Models ranking based on RMSE",
    subtitle = "2015-2025",
    x = "RMSE",
    y = "",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "#2C3E50", color = NA),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  ) +
  # pour voir le texte aux marges
  expand_limits(x = max(df_col_top$RMSE) * 1.15)

############################################################
# Robustness all models without covid
#####################################################

df_eco <- read_xlsx("./Results_Eco_Climat.xlsx") |>
  filter(dates > "2015-01-01") |>
  filter(dates < "2020-02-01" | dates > "2022-01-01")


df_eco_bdf_no_covid <- df_eco |>
  select(PIB_PR, BDF_ALL_M1:BDF_ALL_M3)
df_eco_insee_no_covid <- df_eco |>
  select(PIB_PR, INSEE_ALL_M1:INSEE_ALL_M3)
df_eco_all_no_covid <- df_eco |>
  select(PIB_PR, COMB_ALL_M1:COMB_ALL_M3)


rmse_BDF <- df_eco_bdf_no_covid |>
  na.omit() |>
  summarise(
    Model = "ECO_BDF",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - BDF_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - BDF_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - BDF_ALL_M3)^2))
  )

rmse_INSEE <- df_eco_insee_no_covid |>
  na.omit() |>
  summarise(
    Model = "ECO_INSEE",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - INSEE_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - INSEE_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - INSEE_ALL_M3)^2))
  )

rmse_COMB <- df_eco_all_no_covid |>
  na.omit() |>
  summarise(
    Model = "ECO_ALL",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - COMB_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - COMB_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - COMB_ALL_M3)^2))
  )

rmse_table_no_covid <- bind_rows(rmse_BDF, rmse_INSEE, rmse_COMB)

# Renommer cols
rmse_table_no_covid <- rmse_table_no_covid |>
  rename_with(
    .fn = ~ gsub("RMSE_Mois_", "RMSE_M", .),
    .cols = starts_with("RMSE_Mois_")
  )

rmse_table <- rmse_table_no_covid

metrics_recap_final <- metrics_recap_final |>
  select(Model, starts_with("RMSE"))

metrics_final <- bind_rows(rmse_table, metrics_recap_final_2)


df_metrics_all <- metrics_final |>
  pivot_longer(
    cols = starts_with("RMSE"),
    names_to = "Mois",
    values_to = "RMSE"
  ) |>
  mutate(
    Model = ifelse(Model == "all_txt", "ALL_txt", Model),
    Type = case_when(
      str_detect(Model, "^BDF") ~ "LLM BDF",
      str_detect(Model, "^INSEE") ~ "LLM INSEE",
      str_detect(Model, "^ECO") ~ "ECONOMETRICS",
      TRUE ~ "LLM ALL"
    )
  )

df_metrics_all <- df_metrics_all |>
  mutate(Mois = str_replace(Mois, "RMSE_", ""))

df_col_top <- df_metrics_all |>
  group_by(Mois) |>
  mutate(Model_Ordered = reorder_within(Model, -RMSE, Mois)) |>
  ungroup()

custom_colors <- c(
  "LLM BDF" = "cornflowerblue",
  "LLM INSEE" = "cyan4",
  "LLM ALL" = "cyan",
  "ECONOMETRICS" = "coral1"
)


ggplot(df_col_top, aes(x = RMSE, y = Model_Ordered, fill = Type)) +
  geom_col(width = 0.7, alpha = 0.9) +

  # Affichage de la valeur exacte au bout de la barre
  geom_text(aes(label = round(RMSE, 3)), hjust = -0.2, size = 3.5, fontface = "bold") +

  # Un facet par mois
  facet_wrap(~Mois, scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_manual(
    values = custom_colors
  ) +


  # Légendes
  labs(
    title = "Models ranking based on RMSE",
    subtitle = "2015-2025 without the COVID period",
    x = "RMSE",
    y = "",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "#2C3E50", color = NA),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  ) +
  # pour voir le texte aux marges
  expand_limits(x = max(df_col_top$RMSE) * 1.15)


######################################################################
# Toute la période sans covid
######################################################################


df_eco <- read_xlsx("./Results_Eco_Climat.xlsx") |>
  filter(dates > "2015-01-01") |>
  filter(dates < "2020-02-01" | dates > "2022-01-01")


df_eco_bdf_no_covid <- df_eco |>
  select(PIB_PR, BDF_ALL_M1:BDF_ALL_M3)
df_eco_insee_no_covid <- df_eco |>
  select(PIB_PR, INSEE_ALL_M1:INSEE_ALL_M3)
df_eco_all_no_covid <- df_eco |>
  select(PIB_PR, COMB_ALL_M1:COMB_ALL_M3)


rmse_BDF <- df_eco_bdf_no_covid |>
  na.omit() |>
  summarise(
    Model = "ECO_BDF",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - BDF_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - BDF_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - BDF_ALL_M3)^2))
  )

rmse_INSEE <- df_eco_insee_no_covid |>
  na.omit() |>
  summarise(
    Model = "ECO_INSEE",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - INSEE_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - INSEE_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - INSEE_ALL_M3)^2))
  )

rmse_COMB <- df_eco_all_no_covid |>
  na.omit() |>
  summarise(
    Model = "ECO_ALL",
    RMSE_Mois_1 = sqrt(mean((PIB_PR - COMB_ALL_M1)^2)),
    RMSE_Mois_2 = sqrt(mean((PIB_PR - COMB_ALL_M2)^2)),
    RMSE_Mois_3 = sqrt(mean((PIB_PR - COMB_ALL_M3)^2))
  )

rmse_table_no_covid <- bind_rows(rmse_BDF, rmse_INSEE, rmse_COMB)

# Renommer cols
rmse_table_no_covid <- rmse_table_no_covid |>
  rename_with(
    .fn = ~ gsub("RMSE_Mois_", "RMSE_M", .),
    .cols = starts_with("RMSE_Mois_")
  )


# Modèle LLM
metrics_recap_final_2 <- metrics_recap_final |>
  select(Model, starts_with("RMSE")) |>
  rename_with(
    .fn = ~ gsub("RMSE_Mois_", "RMSE_M", .),
    .cols = starts_with("RMSE_Mois_")
  )

# Joindre
metrics_final <- bind_rows(rmse_table_no_covid, metrics_recap_final_2)


# Préparation graph

df_metrics_all <- metrics_final |>
  filter(Model %in% c("BDF_txt", "INSEE_txt", "ALL_txt", "all_txt", "ECO_ALL", "ECO_BDF", "ECO_INSEE", "ECO_AR")) |>
  pivot_longer(
    cols = starts_with("RMSE"),
    names_to = "Mois",
    values_to = "RMSE"
  ) |>
  mutate(
    Model = ifelse(Model == "all_txt", "ALL_txt", Model),
    Type = case_when(
      str_detect(Model, "^BDF") ~ "LLM BDF",
      str_detect(Model, "^INSEE") ~ "LLM INSEE",
      str_detect(Model, "^ECO") ~ "ECONOMETRICS",
      TRUE ~ "LLM ALL"
    )
  )

# Nettoyage de la variable Mois (RMSE_M1 -> M1)
df_metrics_all <- df_metrics_all |>
  mutate(Mois = str_replace(Mois, "RMSE_", ""))

df_col_top <- df_metrics_all |>
  group_by(Mois) |>
  mutate(Model_Ordered = reorder_within(Model, -RMSE, Mois)) |>
  ungroup()

# Définition des couleurs (reprises de votre dernière requête)
custom_colors <- c(
  "LLM BDF" = "cornflowerblue",
  "LLM INSEE" = "cyan4",
  "LLM ALL" = "cyan",
  "ECONOMETRICS" = "coral1"
)

# Graph

ggplot(df_col_top, aes(x = RMSE, y = Model_Ordered, fill = Type)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = round(RMSE, 3)), hjust = -0.2, size = 3.5, fontface = "bold") +

  # Un facet par mois
  facet_wrap(~Mois, scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_manual(
    values = custom_colors
  ) +

  # Légendes
  labs(
    title = "Model ranking based on RMSE",
    subtitle = "2015-2025 without the COVID-19 period",
    x = "RMSE",
    y = "",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "#2C3E50", color = NA),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  ) +
  expand_limits(x = max(df_col_top$RMSE) * 1.15)


#######################################
# SERIE TEMPORELLE : PIB REEL VS PREVU
#######################################

serie_temp <- left_join(stats_both, pib, by = c("forecast_year", "forecast_quarter")) |>
  # clean les données et mettre en forme les données de pib
  rename(Model = source) |>
  pivot_longer(
    cols = c(median_forecast, PIB_PR),
    names_to = "Metric",
    values_to = "Value"
  ) |>
  mutate(
    Model = ifelse(Metric == "PIB_PR", "PIB_PR", Model)
  ) |>
  filter(
    (Metric == "PIB_PR") | (Model %in% c("BDF", "INSEE") & Metric == "median_forecast")
  ) |>
  # Variables finales
  mutate(
    Line_Type = case_when(
      Model == "PIB_PR" ~ "GDP growth",
      Model == "BDF" ~ "Nowcasts BDF",
      Model == "INSEE" ~ "Nowcasts INSEE",
      TRUE ~ "Autre"
    )
  )

# Graph
ggplot(serie_temp, aes(y = Value, x = as.Date(Date.x), color = Line_Type)) +
  ylim(-0.25, 0.75) +
  geom_line(linewidth = 0.75) +

  # abscisse
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "2 years",
    expand = expansion(mult = 0.02)
  ) +
  # rectangle gris covid
  geom_rect(
    xmin = as.Date("2020-01-01"),
    xmax = as.Date("2022-01-01"),
    ymin = -Inf,
    ymax = Inf,
    fill = "gray80",
    alpha = 0.4,
    color = NA
  ) +
  scale_color_manual(
    name = "Source",
    values = c(
      "GDP growth" = "black",
      "Nowcasts BDF" = "blue",
      "Nowcasts INSEE" = "red"
    )
  ) +

  # Légendes
  labs(
    title = "Forecast vs Actual GDP growth",
    y = "GDP growth (%)",
    x = "YEAR",
    color = "Source"
  ) +
  theme_minimal()
