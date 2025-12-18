# Test cas aléatoire : 

source("Library_Nowcasting_LLM.R")

##########################################
#Initialisation
#########################################

prep_stats <- function(path, src) {
  read_xlsx(path) |> rowwise() |> 
    mutate(median_forecast = median(c_across(starts_with("forecast_")), na.rm=T),
           median_conf = median(c_across(starts_with("confidence_")), na.rm=T), source=src) |>
    select(Date, median_forecast, median_conf, source) |> ungroup()
}


BDF_random <- prep_stats("BDF_text_aleat.xlsx", "BDF")
INSEE_random <- prep_stats("INSEE_text_aleat.xlsx", "INSEE")
BDF_txt <- prep_stats("Final_Results/BDF_text_2020.xlsx", "BDF")
INSEE_txt <- prep_stats("Final_Results/INSEE_text_2020.xlsx", "INSEE")


both_random <- bind_rows(BDF_random, INSEE_random)
both_txt <-  bind_rows(BDF_txt, INSEE_txt)

test_random <- both_random |>
  left_join(both_txt, join_by("Date", "source")) |>
  arrange(Date)

#Préparation des df
df_comparaison <- test_random |>
  rename(
    Forecast_Random = median_forecast.x,
    Conf_Random     = median_conf.x,
    Forecast_Ordered   = median_forecast.y,
    Conf_Ordered       = median_conf.y
  ) |>
  mutate(
    Diff_Forecast = Forecast_Random - Forecast_Ordered,
    Diff_Conf     = Conf_Random - Conf_Ordered
  )

df_forecast <- df_comparaison |>
  select(Date, source, Forecast_Random, Forecast_Ordered) |>
  pivot_longer(cols = c(Forecast_Random, Forecast_Ordered),
               names_to = "Condition", values_to = "Forecast") |>
  mutate(Condition = str_remove(Condition, "Forecast_"))



#############################
# Test sur les moyennes
#############################

ks.test(df_comparaison$Forecast_Ordered, df_comparaison$Forecast_Random)


###################################
#Analyse graphique 
##################################


# Graphique 
ggplot(df_forecast, aes(x = Forecast, fill = Condition)) +
  geom_density(alpha = 0.5, color = NA) + 
   xlim(-1,2) +
  facet_wrap(~source) + 
  scale_fill_manual(values = c("Random" = "#E74C3C", "Ordered" = "#2980B9")) +
  labs(
    title = "Distribution of prevision between random and ordered dates",
    x = "GDP growth forecasted",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "right")




#Boxplot confiance
df_conf <- df_comparaison |>
  select(Date, source, Conf_Random, Conf_Ordered) |>
  pivot_longer(cols = c(Conf_Random, Conf_Ordered),
               names_to = "Condition", values_to = "Confiance") |>
  mutate(Condition = str_remove(Condition, "Conf_"))


ggplot(df_conf, aes(x = Condition, y = Confiance, fill = Condition)) +
  geom_boxplot(width = 0.1, alpha = 0.8) +
  facet_wrap(~source) +
  scale_fill_manual(values = c("Random" = "#E74C3C", "Ordered" = "#2980B9")) +
  labs(
    title = "Confidence level acording to the order",
    y = "Confidence score",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")