#Script : Requête LLM avec enquête en input, de 1 à 3 selon le mois au sein du trimestre

rm(list = ls())  
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")

#######################
#Paramètres spécifiques
#######################

#Systeme prompt
sys_prompt <- system_prompt("Text")

#Initialisation LLM
if (cle_API == "") stop("Clé API Gemini manquante. Ajoute API_KEY_GEMINI dans env/.Renviron")
chat_gemini <- chat_google_gemini( system_prompt = sys_prompt,
                                   base_url = "https://generativelanguage.googleapis.com/v1beta/", 
                                   api_key = cle_API, 
                                   model = "gemini-2.5-pro", 
                                   params(temperature = temp_LLM, max_tokens = 5000)
)

document_folder_BDF <- "docEMC_clean"
document_folder_INSEE <- "INSEE_Scrap"
output_folder_BDF <- "BDF_rolling_files_used"
output_folder_INSEE <- "INSEE_rolling_files_used"


###################################
# Prompts 
###################################


if (english == 1) {
  try(Sys.setlocale("LC_TIME", "English"), silent = TRUE)
  
  #Renvoie le bon dirigeant
  current_boss <- function(type, d) {
    if (type == "BDF") return(BDF_current_boss(d))
    if (type == "INSEE") return(INSEE_current_boss(d))
  }
  
  # 
  prompt_template <- function(type, d, q_trim, y_prev) {
    boss <- current_boss(type, d)
    position <- ifelse(type == "BDF", "Governor of the Banque de France" , "Director General of INSEE")
    current_quarter <- if (q_trim == 1){
      "first"}
    else if (q_trim == 2){
      "second"
    }else if (q_trim == 3){
      "third"}else{
        "fourth"}
    
    
    paste0(
      "Forget the previous instructions and answers. You are ", boss, ", ", position, 
      ", giving a speech about the economic outlook of France. Today is ",
      format(d, "%d %B %Y"), ". ",
      "You will be provided with a document with information about the current state and recent past of the French economy. ",
      "Using ONLY the information in that document and information that was available on or before ", format(d, "%d %B %Y"),
      ", provide a numeric forecast (decimal percent with sign, e.g., +0.3) for French real GDP growth in the ", current_quarter, " quarter of ", y_prev,
      " and a confidence level (integer 0–100). Output EXACTLY in this format on a single line (no extra text):\n",
      "<forecast> (<confidence>)\nExample: +0.3 (80)\n",
      "Do NOT use any information published after ", format(d, "%d %B %Y"), "."
    )
  }
  
} else {
  try(Sys.setlocale("LC_TIME", "French"), silent = TRUE)
  
  current_boss <- function(type, d) {
    if (type == "BDF") return(BDF_current_boss(d))
    if (type == "INSEE") return(INSEE_current_boss(d))
  }
  
  prompt_template <- function(type, d, q_trim, y_prev) {
    boss <- current_boss(type, d)
    position <- ifelse(type == "BDF","Gouverneur de la Banque de France", "Directeur Général de l'INSEE")  
    trimestre_actuel <- if (q_trim == 1){
      "premier"}
    else if (q_trim == 2){
      "second"
    }else if (q_trim == 3){
      "troisième"}else{
        "quatrième"}
    
    
    paste0(
      "Oubliez les instructions et les réponses précédentes. Vous êtes ", boss, ", ", position,
      ", qui prononce un discours sur les perspectives économiques de la France. Nous sommes le ",
      format(d, "%d %B %Y"), ". ",
      "Vous recevrez un document concernant la situation actuelle et passée de l'économie française. ",
      "En utilisant UNIQUEMENT les informations contenues dans ce document et celles disponibles au plus tard le ", format(d, "%d %B %Y"),
      ", fournissez une prévision numérique (pourcentage décimal avec signe, ex. +0.3) de la croissance du PIB réel français pour le ",
      trimestre_actuel, " trimestre ", y_prev,
      " et un niveau de confiance (entier 0-100). Renvoyez EXACTEMENT sur une seule ligne (aucun texte supplémentaire) :\n",
      "<prévision> (<confiance>)\nExemple : +0.3 (80)\n",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
}

####################
# Boucle principale
#####################

forecast_confidence_pattern <- "([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)"
results_BDF <- list()
results_INSEE <- list()

t1 <- Sys.time()

for (dt in as.Date(dates$`Date Prevision`)) {
  current_date <- as.Date(dt)
  
  #sécurité 
  if (!dir.exists(output_folder_BDF)) dir.create(output_folder_BDF, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_folder_INSEE)) dir.create(output_folder_INSEE, recursive = TRUE, showWarnings = FALSE)
  
  # identification du trimestre et du rang dans le trimestre
  m <- month(current_date)
  y <- year(current_date)
  
  if (m == 1) {
    # janvier -> dernier trimestre de l'année précédente
    months_in_quarter <- 3
    year_ref <- y - 1
    q_trim <- 4
  } else {
    q_trim <- ((m - 2) %/% 3) + 1
    year_ref <- y
    months_in_quarter <- ((m - 2) %% 3) + 1
  }
  
  message(sprintf("Date: %s -> T%d (%d) mois dans le trimestre", 
                  format(current_date, "%Y-%m-%d"), q_trim, months_in_quarter))
  
  # construire la séquence des mois du trimestre (ex : q_trim = 1 alor de  Jan-Mar)
  quarter_first_month <- (q_trim - 1) * 3 + 1
  quarter_months <- quarter_first_month:(quarter_first_month + 2)
  # on sélectionne les mois voulus selon la date (ordre ancien->récent)
  months_to_fetch <- quarter_months[1:months_in_quarter]
  
  #Partie BDF
  
  BDF_docs_to_merge <- c()
  current_ref_date <- current_date
  date_pub <- date_publi_prev
  #On va prendre tous les EMC publié à la date ou avant et n'en garder que le nombre souhaité (selon position du mois dans trimestre)
  candidats <- date_pub |>
    filter(date_finale_d <= as.Date(current_ref_date) + 1L)
  last_docs<- candidats |>
    arrange(desc(date_finale_d))
  
  docs_selected <- head(last_docs$fichier, months_in_quarter)
  path_docs <- path_from_docname(docs_selected, folder = document_folder_BDF)
  # chemins PDF complets
  BDF_docs_to_merge <- file.path(path_docs)
  BDF_combined_path <- file.path(output_folder_BDF,
                                 paste0("combined_BDF_", format(current_date, "%Y%m%d"), ".pdf"))
  merge_pdfs(BDF_docs_to_merge, BDF_combined_path)
  
  
  # Paramètre de prévision
  m <- month(current_date)
  y <- year(current_date)
  if (m == 1) {
    q_trim <- 4
    year_prev <- y - 1
  } else {
    q_trim <- ((m - 2) %/% 3) + 1
    year_prev <- y
  }
  
  #BDF
  if (file.exists(BDF_combined_path)) {
    uploaded_bdf <- google_upload(BDF_combined_path,
                                  base_url = "https://generativelanguage.googleapis.com/", 
                                  api_key = cle_API)
    
    prompt_bdf <- prompt_template("BDF", current_date, q_trim, year_prev)
    
    out_bdf <- future_lapply(seq_len(n_repro), function(i) {
      tryCatch(chat_gemini$chat(uploaded_bdf, prompt_bdf), error = \(e) NA_character_)
    }, future.seed = TRUE)
    
    histoires <- sapply(out_bdf, \(x) ifelse(is.list(x), x$text, x))
    parsed <- regmatches(histoires, regexec(forecast_confidence_pattern, histoires))
    df_bdf <- data.frame(Date = current_date, Prompt = prompt_bdf)
    for (i in seq_len(n_repro)) {
      if (length(parsed[[i]]) >= 3) {
        df_bdf[[paste0("forecast_", i)]] <- as.numeric(parsed[[i]][2])
        df_bdf[[paste0("confidence_", i)]] <- as.integer(parsed[[i]][3])
      }
      df_bdf[[paste0("answer_", i)]] <- histoires[i]
    }
    results_BDF[[length(results_BDF) + 1]] <- df_bdf
  }
  
  # INSEE
  #BOUCLE INSEE
  
  INSEE_docs_to_merge <- c()
  months_desc <- rev(months_to_fetch) 
  
  for (mm in months_desc) {
    
    target_year <- year_ref
    target_month <- mm 
    
    # Construire noms attendus AAAA_MM_TYPE.pdf
    emi_file <- file.path(document_folder_INSEE, sprintf("%04d_%02d_EMI.pdf", target_year, target_month))
    ser_file <- file.path(document_folder_INSEE, sprintf("%04d_%02d_SER.pdf", target_year, target_month))
    bat_file <- file.path(document_folder_INSEE, sprintf("%04d_%02d_BAT.pdf", target_year, target_month))
    # ordre demandé : EMI, SER, BAT (si existent) ; on ajoute seulement s'ils existent
    if (file.exists(emi_file)) INSEE_docs_to_merge <- c(INSEE_docs_to_merge, emi_file)
    if (file.exists(ser_file)) INSEE_docs_to_merge <- c(INSEE_docs_to_merge, ser_file)
    if (file.exists(bat_file)) INSEE_docs_to_merge <- c(INSEE_docs_to_merge, bat_file)
  }
  
  INSEE_combined_path <- file.path(output_folder_INSEE,
                                   paste0("combined_INSEE_", format(current_date, "%Y%m%d"), ".pdf"))
  merge_pdfs(INSEE_docs_to_merge, INSEE_combined_path)
  
  if (file.exists(INSEE_combined_path)) {
    uploaded_insee <- google_upload(INSEE_combined_path, 
                                    base_url = "https://generativelanguage.googleapis.com/", 
                                    api_key = cle_API)
    prompt_insee <- prompt_template("INSEE", current_date, q_trim, year_prev)
    
    out_insee <- future_lapply(seq_len(n_repro), function(i) {
      tryCatch(chat_gemini$chat(uploaded_insee, prompt_insee), error = \(e) NA_character_)
    }, future.seed = TRUE)
    
    histoires <- sapply(out_insee, \(x) ifelse(is.list(x), x$text, x))
    parsed <- regmatches(histoires, regexec(forecast_confidence_pattern, histoires))
    df_insee <- data.frame(Date = current_date, Prompt = prompt_insee)
    for (i in seq_len(n_repro)) {
      if (length(parsed[[i]]) >= 3) {
        df_insee[[paste0("forecast_", i)]] <- as.numeric(parsed[[i]][2])
        df_insee[[paste0("confidence_", i)]] <- as.integer(parsed[[i]][3])
      }
      df_insee[[paste0("answer_", i)]] <- histoires[i]
    }
    results_INSEE[[length(results_INSEE) + 1]] <- df_insee
  }
}

# Sauvegarde
df_results_rolling_text_BDF <- bind_rows(results_BDF)
df_results_rolling_text_INSEE <-  bind_rows(results_INSEE)
write.xlsx(df_results_rolling_text_BDF, "Results/BDF_rolling_text.xlsx")
write.xlsx(df_results_rolling_text_INSEE, "Results/INSEE_rolling_text.xlsx")

t2 <- Sys.time()
print(diff(range(t1, t2)))


