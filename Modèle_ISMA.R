# BENCHMARK : MODELE ISMA

#Forme : données de 1990 à fin 2024, régression linéaire de 1990 à 2010 puis nowcasting du PIB de 2010 à fin 2024


#rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")



######################
#PARAMETRES UTILISATEUR
#######################

#Taille fenêtre
window <- 40
#ROLLING WINDOW OU RECURSIF
rolling <- TRUE



#####################
#INITIALISATION
######################
df_PIB_ENQ <- read_xlsx("Data_PIB_ENQ_2.xlsx", sheet = "data_Q")

df_PIB_ENQ <-  df_PIB_ENQ |>
  mutate(PIB_lag = lag(PIB_PR)) |>
  mutate(
    PIB_lag = ifelse(row_number() == 1 & is.na(PIB_lag),
                     mean(PIB_PR),
                     lag(PIB_PR)), 
         dates = as.Date((dates), format = "%Y-%m-%d")) 

####################################
# Choix méthode pour trimestres covid
###################################

covid_treatment <- 1 #  0 pour méthode dummy, 1 pour méthode où l'on intègre pas les données covid dans le training dataset

#Dummy de 2020Q1 à 2021Q4
if (covid_treatment == 0 ){
  df_PIB_ENQ <- df_PIB_ENQ |>
    mutate(COVID_Q1_2020 = ifelse(dates == as.Date("2020-02-01") , 1, 0),
           COVID_Q2_2020 = ifelse(dates == as.Date("2020-05-01"), 1, 0),
           COVID_Q3_2020 = ifelse(dates == as.Date("2020-08-01"), 1, 0),
           COVID_Q2_2021 = ifelse(dates == as.Date("2021-05-01"), 1, 0),
           COVID_Q3_2021 = ifelse(dates == as.Date("2021-08-01"), 1, 0),
           )
  
}


###############################
# Boucle prévision
###############################

#Date de début de training
start_forecast_date <- as.Date("2005-02-01") #Peut être automatisé car là nécessaire de rentrer une date comprise dans df_PIB_Q$dates
## Poistion dans le dataset
first_forecast_row <- which(df_PIB_ENQ$dates >= start_forecast_date)[1]

# Verif pour la date
if (is.na(first_forecast_row)) {
  stop("Date de début de prévision non trouvée dans le dataset.")
}

# Stockage des résultats (dates, PIB_PR et prévisions)
forecast_results <- tibble(
  dates = as.Date(character()), 
  PIB_PR = double(),     
  forecast_M1 = double(),       
  forecast_M2 = double(),       
  forecast_M3 = double())

# Période COVID 
start_covid_period <- as.Date("2020-01-01")
end_covid_period <- as.Date("2021-10-31")

# Boucle de prévision : itérer jusqu'à la fin du dataset depuis la date de training choisie
for (i in first_forecast_row:nrow(df_PIB_ENQ)) {
  
  i_start <- ifelse(rolling == TRUE, i - window, 1)
  # Date du forecast
  current_forecast_date <- df_PIB_ENQ$dates[i]
  
  # Sélection dataset d'entrainement
  if (covid_treatment == 1 && rolling == TRUE){
    i_start <- i - window
    
    repeat{
      
      # Ensemble de départ du training dataset
      potential_training_data <- df_PIB_ENQ[i_start : i-1, ]
      
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
    
    }else if (rolling == TRUE) {
      i_start <- i - window
      training_data <- df_PIB_ENQ[i_start : i-1, ]
    
  }else{
    #Si itératif
    training_data <- df_PIB_ENQ[1 : i-1, ]
    
    if (covid_treatment == 1) {
      training_data <- training_data |>
        filter(!(dates >= start_covid_period & dates <= end_covid_period))
    }
  }
  
  # Estimation du modèle (réestimé à chaque boucle)
  if (covid_treatment == 1) {
    reg_M1 <- lm(PIB_PR ~ PIB_lag + EVLIV_M1 + PREVPRO_M1 + INDIC09, data = training_data)
    reg_M2 <- lm(PIB_PR ~ PIB_lag + EVLIV_M2 + EVLIV_M1 + PREVPRO_M2 + INDIC09, data = training_data)
    reg_M3 <- lm(PIB_PR ~ PIB_lag + EVLIV_M3 + EVLIV_M2 + EVLIV_M1 + INDIC09, data = training_data)
  } else {
    #Méthode avec dummies covid
    reg_M1 <- lm(PIB_PR ~ PIB_lag + EVLIV_M1 + PREVPRO_M1 + INDIC09 + COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021+ COVID_Q3_2021, data = training_data)
    reg_M2 <- lm(PIB_PR ~ PIB_lag + EVLIV_M2 + EVLIV_M1 + PREVPRO_M2 + INDIC09 + COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021+ COVID_Q3_2021 , data = training_data) 
    reg_M3 <- lm(PIB_PR ~ PIB_lag + EVLIV_M3 + EVLIV_M2 + EVLIV_M1 + INDIC09 + COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021 + COVID_Q3_2021, data = training_data)
  }
  
  #Newdata dans predict (on utilise les variables en t pour prévoir le PIB de t)
  forecast_data_current_quarter <- df_PIB_ENQ[i, ]
  
  # Prévision
    forecast_M1_val <- predict(reg_M1, newdata = forecast_data_current_quarter)
    forecast_M2_val <- predict(reg_M2, newdata = forecast_data_current_quarter)
    forecast_M3_val <- predict(reg_M3, newdata = forecast_data_current_quarter)
 
  
  #Stockage des résultats
  forecast_results <- forecast_results |>
    add_row(
      dates = current_forecast_date,
      PIB_PR = df_PIB_ENQ$PIB_PR[i],
      forecast_M1 = forecast_M1_val,
      forecast_M2 = forecast_M2_val,
      forecast_M3 = forecast_M3_val
    ) 
  
}



#Df pour comparaison des résultats

df_ISMA <- forecast_results
 

