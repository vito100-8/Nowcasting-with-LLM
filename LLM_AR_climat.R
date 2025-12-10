#AR augmentés d'indice de climat d'industrie/services

source("Library_Nowcasting_LLM.R")


######################
# PARAMETRES
######################
window <- 40
rolling <- FALSE
covid_treatment <- 1 # 1 = Exclusion, 0 = Dummies Covid

###########################
# PREPARATION DATA CLIMAT
##########################

# Lecture
indic_climat <- read_xlsx("climat_insee_bdf.xlsx")


#Nettoyage
indic_climat <- indic_climat |>
  mutate(dates = ...9, .keep = "unused") |> # On suppose que ...8 est la date
  slice_head(n=143) |>
  mutate(
    dates = as.Date(dates),
    dates = dates + 1 
  ) 

indic_climat_final <- indic_climat |>
  select(dates, starts_with("INSEE"), starts_with("BDF")) |>
  select(dates, INSEE_IND_M1:INSEE_BAT_M3, BDF_IND_M1:BDF_BAT_M3)


#####################
# INITIALISATION PIB & DUMMIES
######################

df_PIB_ENQ <- read_xlsx("Data_PIB_ENQ_3.xlsx", sheet = "PIB")

df_PIB_Prep <- df_PIB_ENQ |>
  select(dates, PIB_PR) |>
  mutate(dates = as.Date(dates, format = "%Y-%m-%d")) |>
  mutate(
    PIB_lag = lag(PIB_PR),
    PIB_lag = ifelse(row_number()==1 & is.na(PIB_lag), mean(PIB_PR, na.rm=T), PIB_lag),
    PIB_lag_2 = lag(PIB_lag) ,
    PIB_lag_2 = ifelse(is.na(PIB_lag_2), mean(PIB_PR, na.rm=T), PIB_lag_2)
  ) |>
  # dummies crises
  mutate(
    DUM96Q1 = ifelse(dates == as.Date("1996-02-01"), 1, 0), 
    DUM08Q4 = ifelse(dates == as.Date("2008-11-01"), 1, 0), 
    DUM09Q1 = ifelse(dates == as.Date("2009-02-01"), 1, 0)  
  )

# Dummy COVID (Pour traitement si treatment == 0)
if (covid_treatment == 0 ){
  df_PIB_Prep <- df_PIB_Prep |>
    mutate(COVID_Q1_2020 = ifelse(dates == as.Date("2020-02-01") , 1, 0),
           COVID_Q2_2020 = ifelse(dates == as.Date("2020-05-01"), 1, 0),
           COVID_Q3_2020 = ifelse(dates == as.Date("2020-08-01"), 1, 0),
           COVID_Q2_2021 = ifelse(dates == as.Date("2021-05-01"), 1, 0),
           COVID_Q3_2021 = ifelse(dates == as.Date("2021-08-01"), 1, 0))
}

######################
# MERGE
######################

df_model <- df_PIB_Prep |>
  left_join(indic_climat_final, by = "dates")

df_model <- df_model |> 
  filter(dates > "1995-01-01")
#######################
#BOUCLE DE PREVISION
#######################

start_forecast_date <- as.Date("2010-02-01")
first_forecast_row <- which(df_model$dates >= start_forecast_date)[1]
if (is.na(first_forecast_row)) stop("Date de début non trouvée.")

# Stockage des résultats
## 5 modèles 
Results_Forecast <- tibble(
  dates = as.Date(character()), 
  PIB_PR = double(),
  
  # AR 
  AR_M1=double(), AR_M2=double(), AR_M3=double(),
  
  # BDF
  BDF_IND_M1=double(), BDF_IND_M2=double(), BDF_IND_M3=double(),
  BDF_ALL_M1=double(), BDF_ALL_M2=double(), BDF_ALL_M3=double(),
  
  # INSEE
  INSEE_IND_M1=double(), INSEE_IND_M2=double(), INSEE_IND_M3=double(),
  INSEE_ALL_M1=double(), INSEE_ALL_M2=double(), INSEE_ALL_M3=double(),
  
  
  # BDF et INSEE
  COMB_IND_M1=double(), COMB_IND_M2=double(), COMB_IND_M3=double(),
  COMB_ALL_M1=double(), COMB_ALL_M2=double(), COMB_ALL_M3=double()

)

# Période COVID 
start_covid_period <- as.Date("2020-02-01")
end_covid_period   <- as.Date("2021-10-31")


for (i in first_forecast_row:nrow(df_model)) {
  
  current_forecast_date <- df_model$dates[i]
  forecast_data_current <- df_model[i, ]
  
  # Sélection du training dataset
  
  if (covid_treatment == 1 && rolling == TRUE){
    i_start <- i - window
    repeat{
      tr_data_raw <- df_model[i_start : (i-1), ]
      training_data <- tr_data_raw |> filter(!(dates >= start_covid_period & dates <= end_covid_period))
      if (nrow(training_data) >= window || i_start <= 1) break 
      i_start <- i_start - 1
    }
  } else if (rolling == TRUE) {
    i_start <- i - window
    training_data <- df_model[i_start : (i-1), ]
  } else {
    # Récursif
    training_data <- df_model[1 : (i-1), ]
    if (covid_treatment == 1) {
      training_data <- training_data |> filter(!(dates >= start_covid_period & dates <= end_covid_period))
    }
  }
  
  #Rajouter dummies covid si besoin
  covid_str <- ""
  if (covid_treatment == 0) {
    if (training_data$dates[i-1] < as.Date("2020-02-01")){
      covid_str <- ""
    }else if (training_data$dates[i-1] < as.Date("2020-05-01")){
      covid_str <- " + COVID_Q1_2020"
    }else if (training_data$dates[i-1] < as.Date("2020-08-01")){
      covid_str <- " + COVID_Q1_2020 + COVID_Q2_2020"
    }else if (training_data$dates[i-1] < as.Date("2021-05-01")){
      covid_str <- " + COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020"
    }else if (training_data$dates[i-1] < as.Date("2021-08-01")){
      covid_str <-  " + COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021"
    }else{
      covid_str <- " + COVID_Q1_2020 + COVID_Q2_2020 + COVID_Q3_2020 + COVID_Q2_2021 + COVID_Q3_2021"
    } 
    
  }
  
  # Estimation
  
  # AR (benchmark)
  f_ar <- paste0("PIB_PR ~ PIB_lag + PIB_lag_2 + DUM96Q1 + DUM08Q4 + DUM09Q1", covid_str)
  fit_ar <- lm(as.formula(f_ar), data = training_data)
  p_ar   <- predict(fit_ar, newdata = forecast_data_current)
  
  
  # Initialisation vecteurs pour BDF et INSEE
  p_bdf_ind <- c(); p_bdf_all <- c()
  p_insee_ind <- c(); p_insee_all <- c()
  p_comb_ind <- c(); p_comb_all <- c()
  
  # BOUCLE SUR LES 3 MOIS 
  for(m in 1:3) {
    
    #BDF Climat
    ## Modèle 1 : climat industrie
    f_bdf_ind <- paste0("PIB_PR ~ PIB_lag + BDF_IND_M", m, covid_str)
    fit <- lm(as.formula(f_bdf_ind), data = training_data)
    p_bdf_ind[m] <- predict(fit, newdata = forecast_data_current)

    ## Modèle 2: tous les climats
    f_bdf_all <- paste0("PIB_PR ~ PIB_lag + BDF_IND_M", m, " + BDF_SV_M", m, "+ BDF_BAT_M",m, covid_str)
    fit <- lm(as.formula(f_bdf_all), data = training_data)
    p_bdf_all[m] <- predict(fit, newdata = forecast_data_current)
    
    
    # INSEE Climat
    ## Modèle 1 : climat industrie
    f_insee_ind <- paste0("PIB_PR ~ INSEE_IND_M", m, " + DUM96Q1 + DUM08Q4", covid_str)
    fit <- lm(as.formula(f_insee_ind), data = training_data)
    p_insee_ind[m] <- predict(fit, newdata = forecast_data_current)
    
    ## Modèle 2: tous les climats
    f_insee_all <- paste0("PIB_PR ~ INSEE_IND_M", m, " + INSEE_SV_M", m, " + INSEE_BAT_M", m, " + DUM96Q1 + DUM08Q4", covid_str)
    fit <- lm(as.formula(f_insee_all), data = training_data)
    p_insee_all[m] <- predict(fit, newdata = forecast_data_current)
  
  
  # BDF et INSEE
  ##Modèle 1 : Climat industrie
  f_comb_ind <- paste0("PIB_PR ~ PIB_lag + BDF_IND_M", m, " + INSEE_IND_M", m, covid_str)
  fit <- lm(as.formula(f_comb_ind), data = training_data)
  p_comb_ind[m] <- predict(fit, newdata = forecast_data_current)
  
  ##Modèle 2 : tous les climats
  f_comb_all <- paste0("PIB_PR ~ PIB_lag + BDF_IND_M", m, " + BDF_SV_M", m, " + BDF_BAT_M", m, 
                       " + INSEE_IND_M", m, " + INSEE_SV_M", m, " + INSEE_BAT_M", m, covid_str)
  fit <- lm(as.formula(f_comb_all), data = training_data)
  p_comb_all[m] <- predict(fit, newdata = forecast_data_current)
}


  # Stockage
  
  Results_Forecast <- Results_Forecast |>
    add_row(
      dates = current_forecast_date,
      PIB_PR = df_model$PIB_PR[i],
      
      # AR 
      AR_M1 = p_ar, AR_M2 = p_ar, AR_M3 = p_ar,
      
      # BDF
      BDF_IND_M1 = p_bdf_ind[1], BDF_IND_M2 = p_bdf_ind[2], BDF_IND_M3 = p_bdf_ind[3],
      BDF_ALL_M1 = p_bdf_all[1], BDF_ALL_M2 = p_bdf_all[2], BDF_ALL_M3 = p_bdf_all[3],
      
      # INSEE
      INSEE_IND_M1 = p_insee_ind[1], INSEE_IND_M2 = p_insee_ind[2], INSEE_IND_M3 = p_insee_ind[3],
      INSEE_ALL_M1 = p_insee_all[1], INSEE_ALL_M2 = p_insee_all[2], INSEE_ALL_M3 = p_insee_all[3],
      
      #BDF et INSEE
      COMB_IND_M1 = p_comb_ind[1], COMB_IND_M2 = p_comb_ind[2], COMB_IND_M3 = p_comb_ind[3],
      COMB_ALL_M1 = p_comb_all[1], COMB_ALL_M2 = p_comb_all[2], COMB_ALL_M3 = p_comb_all[3]
    )
}

# Export
# 1 BDF  - Industrie (Ind)
df_AR_climat_BDF_IND <- Results_Forecast |>
  select(dates, PIB_PR, starts_with("BDF_IND"))

# 2 BDF - Tous les climats (All)
df_AR_climat_BDF_ALL <- Results_Forecast |>
  select(dates, PIB_PR, starts_with("BDF_ALL"))

# 3 INSEE - Industrie (Ind)
df_AR_climat_INSEE_IND <- Results_Forecast |>
  select(dates, PIB_PR, starts_with("INSEE_IND"))

# 4 INSEE - Tous les climats (All)
df_AR_climat_INSEE_ALL <- Results_Forecast |>
  select(dates, PIB_PR, starts_with("INSEE_ALL"))

# 5 COMBINÉ - Industrie (Ind)
df_AR_climat_COMB_IND <- Results_Forecast |>
  select(dates, PIB_PR, starts_with("COMB_IND"))

# 6 COMBINÉ - Tous les climats (All)
df_AR_climat_COMB_ALL <- Results_Forecast |>
  select(dates, PIB_PR, starts_with("COMB_ALL"))

write.xlsx(Results_Forecast, "./Results_Eco_Climat.xlsx")