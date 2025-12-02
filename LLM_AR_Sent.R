#AR(2) avec indicateur de sentiment

source("Library_Nowcasting_LLM.R")


######################
#PARAMETRES UTILISATEUR
#######################

#Taille fenêtre
window <- 40
#ROLLING WINDOW OU RECURSIF
rolling <- FALSE
#Choix traitement covid
covid_treatment <- 1 #  0 pour méthode dummy, 1 pour méthode où l'on intègre pas les données covid dans le training dataset

###########################
#RREPARATION DATA SENTIMENT
##########################

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


#####################
#INITIALISATION
######################

df_PIB_ENQ <- read_xlsx("Data_PIB_ENQ_3.xlsx", sheet = "PIB")


df_PIB_ENQ_AR_Sent <-  df_PIB_ENQ |>
  select(!EVLIV_M1:PREVPRO_M3) |>
  mutate(PIB_lag = lag(PIB_PR),
         PIB_lag_2 = lag(PIB_lag)) |>
  mutate(
    PIB_lag = ifelse(row_number() == 1 & is.na(PIB_lag),
                     mean(PIB_PR),
                     lag(PIB_PR)), 
    PIB_lag_2 = ifelse(row_number() %in% c(1,2) & is.na(PIB_lag_2),
                       mean(PIB_lag),
                       lag(PIB_lag)), 
    dates = as.Date((dates), format = "%Y-%m-%d")) 





#Dummy de 2020Q1 à 2021Q4
if (covid_treatment == 0 ){
  df_PIB_ENQ_AR_Sent <- df_PIB_ENQ_AR_Sent |>
    mutate(COVID_Q1_2020 = ifelse(dates == as.Date("2020-02-01") , 1, 0),
           COVID_Q2_2020 = ifelse(dates == as.Date("2020-05-01"), 1, 0),
           COVID_Q3_2020 = ifelse(dates == as.Date("2020-08-01"), 1, 0),
           COVID_Q2_2021 = ifelse(dates == as.Date("2021-05-01"), 1, 0),
           COVID_Q3_2021 = ifelse(dates == as.Date("2021-08-01"), 1, 0),
    )
  
}

######################
# Merge
######################

# joindre par date
df_model <- df_PIB_ENQ_AR_Sent |>
  left_join(indic_sent_clean, by = "dates")

#######
#Modèle
########

#Date de début de training
start_forecast_date <- as.Date("2015-02-01") #Peut être automatisé car là nécessaire de rentrer une date comprise dans df_PIB_Q$dates
## Poistion dans le dataset
first_forecast_row <- which(df_PIB_ENQ_AR_Sent$dates >= start_forecast_date)[1]

# Verif pour la date
if (is.na(first_forecast_row)) {
  stop("Date de début de prévision non trouvée dans le dataset.")
}

# Stockage des résultats (dates, PIB_PR et prévisions)
AR_Sent_forecast_results <- tibble(
  dates = as.Date(character()), 
  PIB_PR = double(),      
  forecast_AR_M1 = double(),        
  forecast_AR_M2 = double(),        
  forecast_AR_M3 = double()
)

# Période COVID 
start_covid_period <- as.Date("2020-01-01")
end_covid_period <- as.Date("2021-10-31")


for (i in first_forecast_row:nrow(df_model)) {
  
  current_forecast_date <- df_model$dates[i]
  
  #Sélection training dataset
  
  if (covid_treatment == 1 && rolling == TRUE){
    i_start <- i - window
    repeat{
      potential_training_data <- df_model[i_start : (i-1), ]
      training_data <- potential_training_data |>
        filter(!(dates >= start_covid_period & dates <= end_covid_period))
      
      if (nrow(training_data) >= window || i_start <= 1) break 
      i_start <- i_start - 1
    }
    # Formule de base (sans sentiment)
    base_formula_str <- "PIB_PR ~ PIB_lag + PIB_lag_2"
    
  } else if (rolling == TRUE) {
    i_start <- i - window
    training_data <- df_model[i_start : (i-1), ]
    base_formula_str <- "PIB_PR ~ PIB_lag + PIB_lag_2 + COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021 + COVID_Q3_2021"
    
  } else {
    # Récursif
    i_start <- 1 # ou 21 selon ta logique historique
    training_data <- df_model[i_start : (i-1), ]
    
    if (covid_treatment == 1) {
      training_data <- training_data |>
        filter(!(dates >= start_covid_period & dates <= end_covid_period))
      base_formula_str <- "PIB_PR ~ PIB_lag + PIB_lag_2"
    } else {
      base_formula_str <- "PIB_PR ~ PIB_lag + PIB_lag_2 + COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021 + COVID_Q3_2021"
    }
  }
  
  
  # Formule de la régression entière
  
  ## On ajoute la variable de sentiment mensuelle
  form_M1 <- as.formula(paste(base_formula_str, "+ sent_M1"))
  form_M2 <- as.formula(paste(base_formula_str, "+ sent_M2"))
  form_M3 <- as.formula(paste(base_formula_str, "+ sent_M3"))
  
  # Estimation
  reg_M1 <- lm(form_M1, data = training_data)
  reg_M2 <- lm(form_M2, data = training_data)
  reg_M3 <- lm(form_M3, data = training_data)
  
  
  #Prévision
  
  forecast_data_current <- df_model[i, ]
  
  pred_M1 <- predict(reg_M1, newdata = forecast_data_current)
  pred_M2 <- predict(reg_M2, newdata = forecast_data_current)
  pred_M3 <- predict(reg_M3, newdata = forecast_data_current)
  
  #Stockage
  AR_Sent_forecast_results <- AR_Sent_forecast_results |>
    add_row(
      dates = current_forecast_date,
      PIB_PR = df_model$PIB_PR[i],
      forecast_AR_M1 = pred_M1,
      forecast_AR_M2 = pred_M2,
      forecast_AR_M3 = pred_M3
    ) 
}


# Save et export

df_AR_Sent <- AR_Sent_forecast_results
write.xlsx(df_AR_Sent, "Results_AR_Sent.xlsx")