#Model : ISMA et indice de sentiment LLM


  
#rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")


######################
#PREPARATION DATA SENTIMENT
######################

indic_sent <- read_xlsx("Final_Results/ECO_sentiment.xlsx")

# Calcul de la médiane des prévisions de sentiment et mise en forme
indic_sent <- indic_sent |>
  select(Date, starts_with("forecast")) |>
  rowwise() |>
  mutate(median_sent = median(c_across(starts_with("forecast")), na.rm = TRUE), .keep = "unused") 

# Transformation 3 colonnes (M1, M2, M3)
indic_sent_mat <- as.data.frame(
  matrix(
    indic_sent$median_sent, 
    ncol = 3, 
    byrow = TRUE
  )
)

# Récupération des données aux dates voulues (pas de données ISMA pour Q4 2025)
indic_sent_mat <- indic_sent_mat |>
  slice_head(n = nrow(indic_sent_mat) - 1)

colnames(indic_sent_mat) <- c("sent_M1", "sent_M2", "sent_M3")

# Df avec dates trimestrielles
indic_sent$Date <- rollbackward(as.Date(indic_sent$Date), roll_to_first = TRUE)
dates_vec <- indic_sent$Date[seq(1, nrow(indic_sent), by = 3)]
dates_vec <- dates_vec[1:nrow(indic_sent_mat)] 
indic_sent_clean <- cbind(dates = as.Date(dates_vec), indic_sent_mat)


######################
# Preparation ISMA
######################

df_PIB_ENQ <- read_xlsx("Data_PIB_ENQ_3.xlsx", sheet = "PIB")

df_PIB_ENQ <-  df_PIB_ENQ |>
  mutate(PIB_lag = lag(PIB_PR)) |>
  mutate(
    PIB_lag = ifelse(row_number() == 1 & is.na(PIB_lag),
                     mean(PIB_PR, na.rm = TRUE),
                     lag(PIB_PR)), 
    dates = as.Date((dates), format = "%Y-%m-%d")) 


######################
# Merge
######################

# joindre par date
df_model <- df_PIB_ENQ |>
  left_join(indic_sent_clean, by = "dates")

######################
# PARAMETRES UTILISATEUR
#######################

#Taille fenêtre
window <- 40
#ROLLING WINDOW OU RECURSIF
rolling <- FALSE
#Choix traitement covid
covid_treatment <- 1 #  0 pour méthode dummy, 1 pour méthode où l'on intègre pas les données covid dans le training dataset


###############################
# Boucle prévision
###############################

#Date de début de training
start_forecast_date <- as.Date("2015-02-01") 
## Position dans le dataset
first_forecast_row <- which(df_model$dates >= start_forecast_date)[1]

# Verif pour la date
if (is.na(first_forecast_row)) {
  stop("Date de début de prévision non trouvée dans le dataset.")
}

# Stockage des résultats
forecast_results <- tibble(
  dates = as.Date(character()), 
  PIB_PR = double(),      
  forecast_M1 = double(),        
  forecast_M2 = double(),        
  forecast_M3 = double()
)

# Période COVID 
start_covid_period <- as.Date("2020-01-01")
end_covid_period <- as.Date("2021-10-31")

# Boucle de prévision
for (i in first_forecast_row:nrow(df_model)) {
  
  i_start <- ifelse(rolling == TRUE, i - window, 21)
  
  # Date du forecast
  current_forecast_date <- df_model$dates[i]
  
  # séléection training dataset
  
  if (covid_treatment == 1 && rolling == TRUE){
    i_start <- i - window
    
    repeat{
      
      potential_training_data <- df_model[i_start : (i-1), ]
      
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
    training_data <- df_model[i_start :(i-1), ]
    
  }else{
    #Si récursif
    training_data <- df_model[i_start :(i-1), ]
    
    if (covid_treatment == 1) {
      training_data <- training_data |>
        filter(!(dates >= start_covid_period & dates <= end_covid_period))
    }
  }
  
  # Modèle ISMA + Indice de sentiment
  
  if (covid_treatment == 1) {
    # Modèles sans dummy Covid mais avec sentiment
    reg_M1 <- lm(PIB_PR ~ PIB_lag + EVLIV_M1 + PREVPRO_M1 + INDIC09 + sent_M1, data = training_data)
    reg_M2 <- lm(PIB_PR ~ PIB_lag + EVLIV_M2 + EVLIV_M1 + PREVPRO_M2 + INDIC09 + sent_M2, data = training_data)
    reg_M3 <- lm(PIB_PR ~ PIB_lag + EVLIV_M3 + EVLIV_M2 + EVLIV_M1 + INDIC09 + sent_M3, data = training_data)
    
  } else {
    # Méthode avec dummies covid + sentiment
    reg_M1 <- lm(PIB_PR ~ PIB_lag + EVLIV_M1 + PREVPRO_M1 + INDIC09 + sent_M1 + 
                   COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021+ COVID_Q3_2021, data = training_data)
    
    reg_M2 <- lm(PIB_PR ~ PIB_lag + EVLIV_M2 + EVLIV_M1 + PREVPRO_M2 + INDIC09 + sent_M2 + 
                   COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021+ COVID_Q3_2021 , data = training_data) 
    
    reg_M3 <- lm(PIB_PR ~ PIB_lag + EVLIV_M3 + EVLIV_M2 + EVLIV_M1 + INDIC09 + sent_M3 + 
                   COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021 + COVID_Q3_2021, data = training_data)
  }
  
  # Données pour la prévision (i)
  forecast_data_current_quarter <- df_model[i, ]
  
  #PREVISION
  
  forecast_M1_val <- predict(reg_M1, newdata = forecast_data_current_quarter)
  forecast_M2_val <- predict(reg_M2, newdata = forecast_data_current_quarter)
  forecast_M3_val <- predict(reg_M3, newdata = forecast_data_current_quarter)
  
  
  # Stockage des résultats
  forecast_results <- forecast_results |>
    add_row(
      dates = as.Date(current_forecast_date),
      PIB_PR = df_model$PIB_PR[i],
      forecast_M1 = forecast_M1_val,
      forecast_M2 = forecast_M2_val,
      forecast_M3 = forecast_M3_val
    ) 
  
}

#Résultats
df_ISMA_sent <- forecast_results
write.xlsx(df_ISMA_sent, "Final_Results/ECO_ISMA_LLM.xlsx")
