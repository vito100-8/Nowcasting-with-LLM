# Comparaison erreurs 
rm(list = ls())
source("Library_Nowcasting_LLM.R")
source("LLM_AR.R")
source("Modèle_ISMA.R")


err_mbessec_M1 <- read_xlsx("Erreurs_art_victor.xlsx", sheet = "MONTH1")
err_mbessec_M2 <- read_xlsx("Erreurs_art_victor.xlsx", sheet = "MONTH2")
err_mbessec_M3 <- read_xlsx("Erreurs_art_victor.xlsx", sheet = "MONTH3")

err_mbessec <- bind_cols(
  err_mbessec_M1, 
  err_mbessec_M2,
  err_mbessec_M3 )

err_mbessec <- err_mbessec |>
  mutate(
    dates = dates...1,
    AR = AR...2,
    .keep = "unused"
  ) |>
  select(dates, AR, starts_with("ISMA"), starts_with("MF") )|>
  filter(dates < "2025-02-01") 
  

err_ISMA <- df_ISMA |>
  filter(dates >= "2015-02-01") |>
  rowwise() |>
  mutate(error_M1 = PIB_PR - forecast_M1,
         error_M2 = PIB_PR - forecast_M2,
         error_M3 = PIB_PR - forecast_M3,
         dates = dates,
         .keep = "none")

err_AR <- df_AR |>
  filter(dates >= "2015-02-01") |>
  rowwise() |>
  mutate(error_AR = PIB_PR -forecast_AR,
         dates = dates,
         .keep = "none")



all_errors <- err_mbessec |>
  inner_join(err_AR, by = "dates") |>
  inner_join(err_ISMA, by = "dates")


errors_model <- all_errors |>
  mutate(
    diff_AR = error_AR- AR, 
    diff_ISMA_M1 = error_M1 - ISMA_M1,
    diff_ISMA_M2 = error_M2 - ISMA_M2,
    diff_ISMA_M3 = error_M3 - ISMA_M3,
    .keep = "none"
  )
