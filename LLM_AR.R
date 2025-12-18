# BENCHMARK AR(2)

source("Library_Nowcasting_LLM.R")


######################
# PARAMETRES UTILISATEUR
#######################

# Taille fenêtre
window <- 40
# ROLLING WINDOW OU RECURSIF
rolling <- FALSE
# Choix traitement covid
covid_treatment <- 1 #  0 pour méthode dummy, 1 pour méthode où l'on intègre pas les données covid dans le training dataset


#####################
# INITIALISATION
######################

df_PIB_ENQ <- read_xlsx("Data_PIB_ENQ_3.xlsx", sheet = "PIB")


df_PIB_ENQ_AR <- df_PIB_ENQ |>
  select(!EVLIV_M1:PREVPRO_M3) |>
  mutate(
    PIB_lag = lag(PIB_PR),
    PIB_lag_2 = lag(PIB_lag)
  ) |>
  mutate(
    PIB_lag = ifelse(row_number() == 1 & is.na(PIB_lag),
      mean(PIB_PR),
      lag(PIB_PR)
    ),
    PIB_lag_2 = ifelse(row_number() %in% c(1, 2) & is.na(PIB_lag_2),
      mean(PIB_lag),
      lag(PIB_lag)
    ),
    dates = as.Date((dates), format = "%Y-%m-%d")
  )


# Dummy de 2020Q1 à 2021Q4
if (covid_treatment == 0) {
  df_PIB_ENQ_AR <- df_PIB_ENQ |>
    mutate(
      COVID_Q1_2020 = ifelse(dates == as.Date("2020-02-01"), 1, 0),
      COVID_Q2_2020 = ifelse(dates == as.Date("2020-05-01"), 1, 0),
      COVID_Q3_2020 = ifelse(dates == as.Date("2020-08-01"), 1, 0),
      COVID_Q2_2021 = ifelse(dates == as.Date("2021-05-01"), 1, 0),
      COVID_Q3_2021 = ifelse(dates == as.Date("2021-08-01"), 1, 0),
    )
}


#######
# Modèle
########

# Date de début de training
start_forecast_date <- as.Date("2015-02-01") # Peut être automatisé car là nécessaire de rentrer une date comprise dans df_PIB_Q$dates
## Poistion dans le dataset
first_forecast_row <- which(df_PIB_ENQ_AR$dates >= start_forecast_date)[1]

# Verif pour la date
if (is.na(first_forecast_row)) {
  stop("Date de début de prévision non trouvée dans le dataset.")
}

# Stockage des résultats (dates, PIB_PR et prévisions)
AR_forecast_results <- tibble(
  dates = as.Date(character()),
  PIB_PR = double(),
  forecast_AR = double(),
)

# Période COVID
start_covid_period <- as.Date("2020-01-01")
end_covid_period <- as.Date("2021-10-31")


# Boucle de prévision : itérer jusqu'à la fin du dataset depuis la date de training choisie
for (i in first_forecast_row:nrow(df_PIB_ENQ_AR)) {
  # Date du forecast
  current_forecast_date <- df_PIB_ENQ_AR$dates[i]
  i_start <- ifelse(rolling == TRUE, i - window, 21)

  # Sélection dataset d'entrainement
  if (covid_treatment == 1 && rolling == TRUE) {
    i_start <- i - window

    repeat{
      # Ensemble de départ du training dataset
      potential_training_data <- df_PIB_ENQ_AR[i_start:i - 1, ]

      # retirer la période COVID
      training_data <- potential_training_data |>
        filter(!(dates >= start_covid_period & dates <= end_covid_period))

      # Si on a bien le bon nombre d'obs alors on arrête (ou si on ne peut plus reculer)
      if (nrow(training_data) >= window || i_start <= 1) {
        break
      }

      # Recule de l'indice de départ
      i_start <- i_start - 1
    }

    reg_formula <- PIB_PR ~ PIB_lag + PIB_lag_2 + INDIC09
  } else if (rolling == TRUE) {
    i_start <- i - window
    training_data <- df_PIB_ENQ_AR[i_start:i - 1, ]
    reg_formula <- PIB_PR ~ PIB_lag + PIB_lag_2 + INDIC09 +
      COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021 + COVID_Q3_2021
  } else {
    # Si itératif
    training_data <- df_PIB_ENQ_AR[i_start:i - 1, ]
    reg_formula <- PIB_PR ~ PIB_lag + PIB_lag_2 + INDIC09 +
      COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021 + COVID_Q3_2021

    if (covid_treatment == 1) {
      training_data <- training_data |>
        filter(!(dates >= start_covid_period & dates <= end_covid_period))
      reg_formula <- PIB_PR ~ PIB_lag + PIB_lag_2 + INDIC09
    }
  }


  # Estimation du modèle (réestimé à chaque boucle)
  reg_AR <- lm(reg_formula, data = training_data)


  # Newdata dans predict (on utilise les variables en t pour prévoir le PIB de t)
  forecast_data_current_quarter <- df_PIB_ENQ_AR[i, ]

  # Prévision
  forecast_AR_val <- predict(reg_AR, newdata = forecast_data_current_quarter)


  # Stockage des résultats
  AR_forecast_results <- AR_forecast_results |>
    add_row(
      dates = current_forecast_date,
      PIB_PR = df_PIB_ENQ_AR$PIB_PR[i],
      forecast_AR = forecast_AR_val,
    )
}


# Df pour comparaison des résultats

df_AR <- AR_forecast_results

write.xlsx(df_AR, "Results_AR.xlsx")
