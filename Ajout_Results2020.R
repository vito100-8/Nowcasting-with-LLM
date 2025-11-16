# Insertion résultats no_text dans fichiers de résultats
# Fichiers stockés dans le dossier final_results

rm(list=ls())

# Importation des résultats

# no_text
df_BDF_2020 <- read_excel('Results/BDF_noText2.xlsx')
df_BDF_2020 <- subset(df_BDF_2020,Date>"2019-12-08")
df_INSEE_2020 <- read_excel('Results/INSEE_noText2.xlsx')
df_INSEE_2020 <- subset(df_INSEE_2020,Date>"2019-12-08")


#============================================
# Correction BDF_noText et INSEE_noText 
df_BDF <- read_excel('Results/BDF_noText.xlsx')
df_INSEE <- read_excel('Results/INSEE_noText.xlsx')

# remplace les dates communes de 2020 et ajoute les dates manquantes
df_final_BDF <- df_BDF %>%
  rows_upsert(df_BDF_2020, by = "Date") %>%
  arrange(Date)

df_final_INSEE <- df_INSEE %>%
  rows_upsert(df_INSEE_2020, by = "Date") %>%
  arrange(Date)

write.xlsx(df_final_BDF, file = "Final_Results/BDF_noText_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)

write.xlsx(df_final_INSEE, file = "Final_Results/INSEE_noText_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)


#============================================
# Correction BDF_text et INSEE_text
df_BDF <- read_excel('Results/BDF_text.xlsx')   #1
df_INSEE <- read_excel('Results/INSEE_text.xlsx')   #2

# remplace les dates communes de 2020 et ajoute les dates manquantes
df_final_BDF <- df_BDF %>%
  rows_upsert(df_BDF_2020, by = "Date") %>%
  arrange(Date)

df_final_INSEE <- df_INSEE %>%
  rows_upsert(df_INSEE_2020, by = "Date") %>%
  arrange(Date)

write.xlsx(df_final_BDF, file = "Final_Results/BDF_text_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #3

write.xlsx(df_final_INSEE, file = "Final_Results/INSEE_Text_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #4


# =============================================
# Correction BDF_excel et INSEE_excel
df_BDF <- read_excel('Results/BDF_excel.xlsx')   #1
df_INSEE <- read_excel('Results/INSEE_excel.xlsx')   #2

# remplace les dates communes de 2020 et ajoute les dates manquantes
df_final_BDF <- df_BDF %>%
  rows_upsert(df_BDF_2020, by = "Date") %>%
  arrange(Date)

df_final_INSEE <- df_INSEE %>%
  rows_upsert(df_INSEE_2020, by = "Date") %>%
  arrange(Date)

write.xlsx(df_final_BDF, file = "Final_Results/BDF_excel_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #3

write.xlsx(df_final_INSEE, file = "Final_Results/INSEE_excel_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #4


# =============================================
# Correction BDF_all_text et INSEE_all_text
df_BDF <- read_excel('Results/BDF_all.xlsx')   #1
df_INSEE <- read_excel('Results/INSEE_all.xlsx')   #2

# remplace les dates communes de 2020 et ajoute les dates manquantes
df_final_BDF <- df_BDF %>%
  rows_upsert(df_BDF_2020, by = "Date") %>%
  arrange(Date)

df_final_INSEE <- df_INSEE %>%
  rows_upsert(df_INSEE_2020, by = "Date") %>%
  arrange(Date)

write.xlsx(df_final_BDF, file = "Final_Results/BDF_all_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #3

write.xlsx(df_final_INSEE, file = "Final_Results/INSEE_all_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #4



# FICHIER VICTOR => format date différents

# =============================================
# Correction BDF_rolling_text et INSEE_rolling_text
df_BDF <- read_excel('Results/BDF_rolling_text.xlsx')   #1
df_INSEE <- read_excel('Results/INSEE_rolling_text.xlsx')   #2

# Pb type de date différent
df_BDF_2020$Date <- as.POSIXct(df_BDF_2020$Date, tz = "UTC")
df_INSEE_2020$Date <- as.POSIXct(df_INSEE_2020$Date, tz = "UTC")

# remplace les dates communes de 2020 et ajoute les dates manquantes
df_final_BDF <- df_BDF %>%
  rows_upsert(df_BDF_2020, by = "Date") %>%
  arrange(Date)

df_final_INSEE <- df_INSEE %>%
  rows_upsert(df_INSEE_2020, by = "Date") %>%
  arrange(Date)

write.xlsx(df_final_BDF, file = "Final_Results/BDF_rolling_text_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #3

write.xlsx(df_final_INSEE, file = "Final_Results/INSEE_rolling_Text_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #4


# # =============================================
# # Correction BDF_just_Text et INSEE_just_Text 
# df_BDF <- read_excel('Results/BDF_just_text.xlsx')   #1
# df_INSEE <- read_excel('Results/INSEE_just_text.xlsx')   #2
# 
# # remplace les dates communes de 2020 et ajoute les dates manquantes
# df_final_BDF <- df_BDF %>%
#   rows_upsert(df_BDF_2020, by = "Date") %>%
#   arrange(Date)
# 
# df_final_INSEE <- df_INSEE %>%
#   rows_upsert(df_INSEE_2020, by = "Date") %>%
#   arrange(Date)
# 
# write.xlsx(df_final_BDF, file = "Final_Results/BDF_just_text_2020.xlsx", 
#            sheetName = 'prevision', rowNames = FALSE)  #3
# 
# write.xlsx(df_final_INSEE, file = "Final_Results/INSEE_just_Text_2020.xlsx", 
#            sheetName = 'prevision', rowNames = FALSE)  #4


