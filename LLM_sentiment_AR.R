#Modèle AR augmenté d'un indice de sentiment LLM

## Constrution : 

#   - 1995- 2014 : sentiment obtenu via no_text
#   - 2915-2025 : sentiment obtenu via text
#   - covid : no_text

source("Library_Nowcasting_LLM.R")


######################################
#Initialisation 
######################################

#Dummy pour traiter covid
covid_treatment <- 1 #1 = on estime la période sans les données covid dans le dataset d'entraînement, 0 = méthode dummy

# Date de début de la prévision 
start_forecast_date <- as.Date("2015-02-01")

#Ouvrir et traitement fichier sentiment
df_sent <- read_xlsx("Indices_sentiment.xlsx")

df_sent <- df_sent |>
  mutate(dates = as.Date(...9), .keep = "unused", na.rm = TRUE) |>
  select(dates, starts_with("BDF_SENT"), starts_with( "INSEE_SENT")) 

df_sent <- df_sent|> filter(!is.na(INSEE_SENT_M1))

df_sent$dates <- rollbackward(df_sent$dates, roll_to_first = TRUE  )

df_sent$dates[61:123 ] <- rollbackward(rollbackward(df_sent$dates[61:123 ] ), roll_to_first = TRUE)
  
#OUVRIR FICHIER PIB
pib <- read_xlsx("Data_BDF_INSEE.xlsx", sheet = "trimestriel")

#Join
df_model <- df_sent |>
  left_join(pib, by = "dates")


####################################
#Préparation du modèle
#####################################
# Modèles : 
# BDF   : PIB(t) = c0 + c1 * PIB(t-1) + c2 * SENT_BDF(i,t)     + c3 * DUM96Q1(t) + c4 * DUM08Q4(t) + c5 * DUM09Q1(t) + eps(t) 
# INSEE : PIB(t) = c0 + c1 * PIB(t-1) + c2 * SENT_INSEE(i,t) + c3 * DUM96Q1(t) + c4  * DUM08Q4(t) + c5 * DUM09Q1(t) + eps(t) 

df_model <- df_model |>
  mutate(PIB_lag = lag(PIB_PR))|>
  mutate(PIB_lag = ifelse(is.na(PIB_lag),
                     mean(PIB_PR),
                     lag(PIB_PR)),
    #Dummies
    DUM96Q1 = ifelse(year(dates) == 1996 & quarter(dates) == 1, 1, 0),
    DUM08Q4 = ifelse(year(dates) == 2008 & quarter(dates) == 4, 1, 0),
    DUM09Q1 = ifelse(year(dates) == 2009 & quarter(dates) == 1, 1, 0))

    if (covid_treatment == 0 ){
      df_model <- df_model |>
        mutate(
          COVID_Q1_2020 = ifelse(dates == as.Date("2020-02-01"), 1, 0),
          COVID_Q2_2020 = ifelse(dates == as.Date("2020-05-01"), 1, 0),
          COVID_Q3_2020 = ifelse(dates == as.Date("2020-08-01"), 1, 0),
          COVID_Q2_2021 = ifelse(dates == as.Date("2021-05-01"), 1, 0),
          COVID_Q3_2021 = ifelse(dates == as.Date("2021-08-01"), 1, 0)
        )
    }
    
##############################################
# Initialisation de la boucle
##############################################

# ligne de départ
first_forecast_row <- which(df_model$dates >= start_forecast_date)[1]
if (is.na(first_forecast_row)) stop("Date de début de prévision non trouvée.")

# Liste des indicateurs 
cols_bdf   <- names(df_model)[grep("BDF_SENT", names(df_model))]
cols_insee <- names(df_model)[grep("INSEE_SENT", names(df_model))]

#Df qui stockera les résultats 
Results_Forecast_Sent <- df_model |> 
  select(dates, PIB_PR) |> 
  filter(dates >= start_forecast_date)


# Initialiser les colonnes de prévision
for(col in c(cols_bdf, cols_insee)) {
  Results_Forecast_Sent[[paste0("Prev_", col)]] <- NA
}

# période à exclure
start_covid_period <- as.Date("2020-01-01")
end_covid_period   <- as.Date("2021-10-31")
    

###############################################################
# Boucle principale
##############################################################

for (i in first_forecast_row:nrow(df_model)) {
  
  current_date <- df_model$dates[i]
  
  # TRAINING DATA (Récursif: de 1 à t-1)
  training_raw <- df_model[1:(i-1), ]
  
  if (covid_treatment == 1) {
    training_data <- training_raw |>
      filter(!(dates >= start_covid_period & dates <= end_covid_period))
  } else {
    training_data <- training_raw
  }
  
  
  
  # Chaîne de caractères pour les dummies Covid (si nécessaire)
  str_covid <- ""
  if (covid_treatment == 0) {
    str_covid <- "+ COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021 + COVID_Q3_2021"
  }
  

  # ESTIMATION DES MODÈLES BDF
  for (var_bdf in cols_bdf) {
    try({
      # formule régression BDF
      f_bdf <- paste0("PIB_PR ~ PIB_lag + ", var_bdf, " + DUM96Q1 + DUM08Q4 + DUM09Q1 ", str_covid)
      
      # Estimation
      model_bdf <- lm(as.formula(f_bdf), data = training_data)
      
      # Prévision
      forecast_data <- df_model[i, ]
      pred <- predict(model_bdf, newdata = forecast_data)
      
      # Stockage
      Results_Forecast_Sent[Results_Forecast_Sent$dates == current_date, paste0("Prev_", var_bdf)] <- pred
    }, silent = TRUE)
  }
  

  # ESTIMATION DES MODÈLES INSEE

  for (var_insee in cols_insee) {
    try({
      #formule  régression INSEE
      f_insee <- paste0("PIB_PR ~ PIB_lag + ", var_insee, " + DUM96Q1 + DUM08Q4 + DUM09Q1 ", str_covid)
      
      # Estimation
      model_insee <- lm(as.formula(f_insee), data = training_data)
      
      # Prévision
      pred <- predict(model_insee, newdata = forecast_data)
      
      # Stockage
      Results_Forecast_Sent[Results_Forecast_Sent$dates == current_date, paste0("Prev_", var_insee)] <- pred
    }, silent = TRUE)
  }
  
}

######################################
# Sauvegarde en xlsx
######################################
BDF_Sent <- Results_Forecast_Sent |>
  select(dates, Prev_BDF_SENT_M1:Prev_BDF_SENT_M3)
INSEE_Sent <- Results_Forecast_Sent|>
  select(dates, Prev_INSEE_SENT_M1:Prev_INSEE_SENT_M3)

write.xlsx(BDF_Sent, "Final_Results/SENT_BDF_BDF.xlsx")
write.xlsx(INSEE_Sent, "Final_Results/SENT_INSEE_BDF.xlsx")
