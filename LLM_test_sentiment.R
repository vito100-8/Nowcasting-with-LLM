# Indicateur de sentiment LLM #

# Description : Fournir au LLM les enquêtes de conjoncture BDF & INSEE et lui demander un indice de sentiment de -4 à +4


rm(list = ls())
source("Library_Nowcasting_LLM.R")
source("LLM_functions.R")
source("Script_dates_prev.R")
source("Parametres_generaux.R")
dates <- dates3 # 2010 à 2020 et 2020 complet

#######################
# Paramètres spécifiques
#######################


# Systeme prompt
sys_prompt <- system_prompt("Text")

# Initialisation LLM
if (cle_API == "") stop("Clé API Gemini manquante. Ajoute API_KEY_GEMINI dans env/.Renviron")
chat_gemini <- chat_google_gemini(
  system_prompt = sys_prompt,
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = cle_API,
  model = "gemini-2.5-pro",
  params(temperature = temp_LLM, max_tokens = 5000)
)

document_folder_BDF <- "docEMC_clean"
document_folder_INSEE <- "INSEE_Scrap"


###################################
# Prompts
###################################

if (english == 1) {
  try(Sys.setlocale("LC_TIME", "English"), silent = TRUE)
  #
  prompt_template <- function(d, q_trim, y_prev) {
    current_quarter <- if (q_trim == 1) {
      "first"
    } else if (q_trim == 2) {
      "second"
    } else if (q_trim == 3) {
      "third"
    } else {
      "fourth"
    }


    paste0(
      "Forget the previous instructions and answers. You are an economist giving a speech about the economic outlook of France. Today is ",
      format(d, "%d %B %Y"), ". ",
      "You will be provided with a document with information about the current state and recent past of the French economy. ",
      "Using ONLY the information in that document and information that was available on or before ", format(d, "%d %B %Y"),
      ", provide a score between -4 and +4 assessing the performance of French economic activity in the ", current_quarter, " quarter of ", y_prev,
      " and a confidence level (integer 0–100). ",
      "-4 means significantly contracting activity and +4 means a significantly expanding activity.",
      "Output EXACTLY in this format on a single line (no extra text):\n",
      "<score> (<confidence>)\nExample: +3.2 (80)\n",
      "Do NOT use any information published after ", format(d, "%d %B %Y"), "."
    )
  }
} else {
  try(Sys.setlocale("LC_TIME", "French"), silent = TRUE)
  prompt_template <- function(d, q_trim, y_prev) {
    trimestre_actuel <- if (q_trim == 1) {
      "premier"
    } else if (q_trim == 2) {
      "second"
    } else if (q_trim == 3) {
      "troisième"
    } else {
      "quatrième"
    }


    paste0(
      "Oubliez les instructions et les réponses précédentes. Vous êtes un économiste qui prononce un discours sur les perspectives économiques de la France. Nous sommes le ",
      format(d, "%d %B %Y"), ". ",
      "Vous recevrez un document concernant la situation actuelle et passée de l'économie française. ",
      "En utilisant UNIQUEMENT les informations contenues dans ce document et celles disponibles au plus tard le ", format(d, "%d %B %Y"),
      ", fournissez un score compris entre -4 et +4 évaluant l'activité économique française pour le ",
      trimestre_actuel, " trimestre ", y_prev,
      " et un niveau de confiance (entier de 0–100).. ",
      "Renvoyez EXACTEMENT sur une seule ligne (aucun texte supplémentaire) :\n",
      "<score> (<confiance>)\nExemple : +3.2 (80)\n",
      ". -4 décrit une contraction significative de l'activité et +4 décrit une expansion significative de l'activité.",
      "N'utilisez AUCUNE information publiée après le ", format(d, "%d %B %Y"), "."
    )
  }
}


###################################
# Boucle principale BDF
###################################

# Création chemin d'accès
dir.create("Both_files_used", showWarnings = FALSE)

# Forecast regex pattern qui sera appelé dans la boucle pour parse
forecast_confidence_pattern <- "([+-]?\\d+\\.?\\d*)\\s*\\(\\s*(\\d{1,3})\\s*\\)"

# Creation de la list contenant les résultats
df_results <- list()
row_id <- 1

t1 <- Sys.time()

for (dt in as.Date(dates$`Date Prevision`)) {
  current_date <- as.Date(dt)

  # Trouver le bon pdf et son path
  docname <- get_next_doc(current_date)
  pdf_path <- path_from_docname(docname, folder = document_folder_BDF)

  # if (is.null(pdf_path)) {
  #   warning("No PDF found for date ", current_date, " — skipping.")
  #   next
  # }


  # Trouver les bons pdf, le chemin d'accès et les concaténer
  emi_path <- get_last_insee_docs_by_type(current_date, "EMI", document_folder_INSEE)
  ser_path <- get_last_insee_docs_by_type(current_date, "SER", document_folder_INSEE)
  bat_path <- get_last_insee_docs_by_type(current_date, "BAT", document_folder_INSEE)

  ## concaténation des documents dans le chemin d'accès spécifié
  all_docs_to_combine <- c(emi_path, ser_path, bat_path, pdf_path)
  combined_pdf_path <- file.path("./Both_files_used/", paste0("combined_", format(current_date, "%Y%m%d"), ".pdf"))
  input_docs <- merge_pdfs(all_docs_to_combine, combined_pdf_path)

  # Chargement du pdf souhaité
  uploaded_doc <- google_upload(
    input_docs,
    base_url = "https://generativelanguage.googleapis.com/",
    api_key = cle_API
  )
  # Initialisation des dates
  current_date <- as.Date(dt)
  mois_index <- as.integer(format(current_date, "%m"))
  year_current <- as.integer(format(current_date, "%Y"))
  trimestre_index <- if (mois_index %in% c(1, 11, 12)) 4 else if (mois_index %in% 2:4) 1 else if (mois_index %in% 5:7) 2 else 3
  year_prev <- if (mois_index == 1 && trimestre_index == 4) year_current - 1 else year_current
  prompt_text <- prompt_template(
    current_date, trimestre_index,
    year_prev
  )
  print(prompt_text)
  # appel à Gemini en intégrant le document voulu
  out_list <- future_lapply(seq_len(n_repro), function(i) {
    tryCatch(
      {
        resp <- chat_gemini$chat(uploaded_doc, prompt_text)
        return(resp)
      },
      error = function(e) {
        message("API error: ", conditionMessage(e))
        return(NA_character_)
      }
    )
  }, future.seed = TRUE)

  # Parse les résultats
  histoires <- sapply(out_list, function(x) {
    if (is.list(x) && !is.null(x$text)) {
      return(x$text)
    } else if (is.character(x)) {
      return(x)
    } else {
      return(NA_character_)
    }
  })
  parsed_list <- lapply(histoires, function(txt) {
    if (is.null(txt) || length(txt) == 0) {
      return(list(forecast = NA_real_, confidence = NA_integer_, raw = NA_character_))
    }
    m <- regmatches(txt, regexec(forecast_confidence_pattern, txt))
    if (length(m[[1]]) >= 3) {
      list(forecast = as.numeric(m[[1]][2]), confidence = as.integer(m[[1]][3]), raw = txt)
    } else {
      list(forecast = NA_real_, confidence = NA_integer_, raw = txt)
    }
  })

  # Df des résultats
  df_both <- data.frame(Date = as.character(current_date), Prompt = prompt_text, stringsAsFactors = FALSE)
  for (i in seq_len(n_repro)) {
    df_both[[paste0("forecast_", i)]] <- parsed_list[[i]]$forecast
    df_both[[paste0("confidence_", i)]] <- parsed_list[[i]]$confidence
    df_both[[paste0("answer_", i)]] <- parsed_list[[i]]$raw
  }

  df_results[[row_id]] <- df_both
  row_id <- row_id + 1
  Sys.sleep(0.5)
}

# réunir les prévisions pour chaque date
df_indice <- do.call(rbind, df_results)

# Enregistrement
write.xlsx(df_indice, file = "Results/ECO_sentiment.xlsx", sheetName = "prevision", rowNames = FALSE)
print("Enregistré: ECO_sentiment.xlsx \n")

t2 <- Sys.time()
print(diff(range(t1, t2)))
