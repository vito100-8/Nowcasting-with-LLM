# SCRIPT PARAMETRES GENERAUX


rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")


##############################
# Répertoire de travail actif
#############################


here::i_am("Parametres_generaux.R")

setwd(dirname(getActiveDocumentContext()$path))

load_dot_env('.env')


###################################################
#INITIALISATION PARAMETRES
###################################################


#Paramètres généraux
english <- 1 # 1 si prompt en anglais
temp_LLM <- 0.7  # Niveau de créativité des réponses 0.3/0.7/1.5 (castro-Leibovici)
n_repro <- 2  # Nombre de prévisions générées par date


# Initialisation des dates
#df_date <- as.Date(c("2012-01-03")) #POUR TESTER : à changer manuellement

#Dates utilisées
dates <- read_xlsx(here("dates_prev.xlsx")) #possible aussi de prendre df_date (dates <- df_date)

# API Key (pour ellmer on utilise API_KEY_GEMINI)
cle_API <- Sys.getenv("API_KEY_GEMINI")

########################################
# Choix du système prompt
#######################################

system_prompt <- function(x){
  if (x == "Text"){
    txt_prompt <- ifelse(english == 1,
                         "You will act as the economic agent you are told to be. Answer based on your knowledge and the document provided. You will use only the information available as of the forecast date, do not invent facts." ,
                         "Vous allez incarner des agents économiques spécifiés. Répondez aux questions à l'aide de vos connaissances et du document fourni. Vous n'utiliserez que l'information disponible à la date du jour de la prévision, n'inventez pas de faits."
    )
  }else if (x == "noText"){
    txt_prompt <- ifelse(english == 1,
                         "You will act as the economic agent you are told to be. Answer based on your knowledge and researches, do not invent facts." ,
                         "Vous allez incarner des agents économiques spécifiés. Répondez aux questions à l'aide de vos connaissances et de vos recherches, n'inventez pas de faits.")
    
  }else if(x== "all"){
    txt_prompt <- ifelse(english == 1,
                         "You will act as the economic agent you are told to be. Answer based on your knowledge, the document provided and the information provided in the prompt. You will use only the information available as of the forecast date, do not invent facts." ,
                         "Vous allez incarner des agents économiques spécifiés. Répondez aux questions à l'aide de vos connaissances, du document fourni et des informations contenues dans le prompt utilisateur. Vous n'utiliserez que l'information disponible à la date du jour de la prévision, n'inventez pas de faits."
    )
    
  }else if(x== "justText"){
    txt_prompt <- ifelse(english == 1,
                         "You will act as the economic agent you are told to be. Answer based solely on the document provided. Do not invent facts." ,
                         "Vous allez incarner des agents économiques spécifiés. Répondez aux questions uniquement à l'aide du document fourni. N'inventez pas de faits."
    )
  }
  return(txt_prompt)
}





