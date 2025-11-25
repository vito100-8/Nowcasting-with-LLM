# Insertion résultats no_text dans fichiers de résultats
# Fichiers stockés dans le dossier final_results

rm(list=ls())
source("Library_Nowcasting_LLM.R")

# Importation des résultats

# no_text  => 2020-02 à 07, 2020-09 et 2021-01
# CAS anglais
df_BDF_2020 <- read_excel('Results/BDF_noText_sansEMC.xlsx')
df_INSEE_2020 <- read_excel('Results/INSEE_noText_sansEMC.xlsx')
# CAS français
df_BDF_2020_FR <- read_excel('Results/BDF_noText_sansEMC_FR.xlsx')
df_INSEE_2020_FR <- read_excel('Results/INSEE_noText_sansEMC_FR.xlsx')

# si on veut tout remplacer de janv 2020 à janv 2021 (pour rolling) 
#df_BDF_2020 <- read_excel('Results/BDF_noText2.xlsx')
#df_INSEE_2020 <- read_excel('Results/INSEE_noText2.xlsx')
#df_BDF_2020 <- subset(df_BDF_2020,Date>"2019-12-08")
#df_INSEE_2020 <- subset(df_INSEE_2020,Date>"2019-12-08")


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

#============================================
# Correction BDF_text_FR et INSEE_text_FR
df_BDF <- read_excel('Results/BDF_text_FR.xlsx')   #1
df_INSEE <- read_excel('Results/INSEE_text_FR.xlsx')   #2

# remplace les dates communes de 2020 et ajoute les dates manquantes
# ON UTILISE LES RESULTATS DE NO_TEXT en FRANCAIS
df_final_BDF <- df_BDF %>%
  rows_upsert(df_BDF_2020_FR, by = "Date") %>%
  arrange(Date)

df_final_INSEE <- df_INSEE %>%
  rows_upsert(df_INSEE_2020_FR, by = "Date") %>%
  arrange(Date)

write.xlsx(df_final_BDF, file = "Final_Results/BDF_text_FR_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #3

write.xlsx(df_final_INSEE, file = "Final_Results/INSEE_Text_FR_2020.xlsx", 
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



# =============================================
# Correction BDF_just_Text et INSEE_just_Text
df_BDF <- read_excel('Results/BDF_just_text.xlsx')   #1
df_INSEE <- read_excel('Results/INSEE_just_text.xlsx')   #2

# remplace les dates communes de 2020 et ajoute les dates manquantes
df_final_BDF <- df_BDF %>%
  rows_upsert(df_BDF_2020, by = "Date") %>%
  arrange(Date)

df_final_INSEE <- df_INSEE %>%
  rows_upsert(df_INSEE_2020, by = "Date") %>%
  arrange(Date)

write.xlsx(df_final_BDF, file = "Final_Results/BDF_just_text_2020.xlsx",
           sheetName = 'prevision', rowNames = FALSE)  #3

write.xlsx(df_final_INSEE, file = "Final_Results/INSEE_just_Text_2020.xlsx",
           sheetName = 'prevision', rowNames = FALSE)  #4


# =============================================
# Correction BDF_excel_error et INSEE_excel_error
df_BDF <- read_excel('Results/BDF_excel_error.xlsx')   #1
df_INSEE <- read_excel('Results/INSEE_excel_error.xlsx')   #2

# remplace les dates communes de 2020 et ajoute les dates manquantes
df_final_BDF <- df_BDF %>%
  rows_upsert(df_BDF_2020, by = "Date") %>%
  arrange(Date)

df_final_INSEE <- df_INSEE %>%
  rows_upsert(df_INSEE_2020, by = "Date") %>%
  arrange(Date)

write.xlsx(df_final_BDF, file = "Final_Results/BDF_excel_error_2020.xlsx",
           sheetName = 'prevision', rowNames = FALSE)  #3

write.xlsx(df_final_INSEE, file = "Final_Results/INSEE_excel_error_2020.xlsx",
           sheetName = 'prevision', rowNames = FALSE)  #4



# FICHIER VICTOR => format date différents
# =============================================
# Correction BDF_rolling_text et INSEE_rolling_text
# TRAITEMENT PARTICULIER POUR ROLLING 
   # => ON REMPLACE TOUT A PARTIR DE 2020-02 PAR _NO_TEXT
   # => ON REMPLACE les 4 dates avec EMC par _TEXT
df_BDF <- read_excel('Results/BDF_rolling_text.xlsx')   #1
df_INSEE <- read_excel('Results/INSEE_rolling_text.xlsx')   #2

# ETAPE 1 : on remplace tout par _no_tex
  df_BDF_2020 <- read_excel('Results/BDF_noText2.xlsx')
  df_INSEE_2020 <- read_excel('Results/INSEE_noText2.xlsx')
  df_BDF_2020 <- subset(df_BDF_2020,Date>"2020-01-09"&Date<"2020-12-13"|Date=="2021-01-12")
  df_INSEE_2020 <- subset(df_INSEE_2020,Date>"2020-01-09"&Date<"2020-12-13"|Date=="2021-01-12")
  
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

  
# ETAPE 2 : on affine en remplaçant les 4 dates avec EMC par _text
#  df_BDF_2020bis <- read_excel('Results/BDF_Text_avecEMC.xlsx')
#  df_INSEE_2020bis <- read_excel('Results/INSEE_Text_avecEMC.xlsx')
  
  df_BDF_2020bis <- read_excel('Results/BDF_Text.xlsx')
  df_BDF_2020bis <- subset(df_BDF_2020bis,Date=="2020-08-09"|Date=="2020-10-07"|Date=="2020-11-08")
  df_INSEE_2020bis <- read_excel('Results/INSEE_Text.xlsx')
  df_INSEE_2020bis <- subset(df_INSEE_2020bis,Date=="2020-08-09"|Date=="2020-10-07"|Date=="2020-11-08")

    # Pb type de date différent
  df_BDF_2020bis$Date <- as.POSIXct(df_BDF_2020bis$Date, tz = "UTC")
  df_INSEE_2020bis$Date <- as.POSIXct(df_INSEE_2020bis$Date, tz = "UTC")
  
  # remplace les dates communes de 2020 (avec EMC)
  df_final_BDF <- df_final_BDF %>%
    rows_upsert(df_BDF_2020bis, by = "Date") %>%
    arrange(Date)
  
  df_final_INSEE <- df_final_INSEE %>%
    rows_upsert(df_INSEE_2020bis, by = "Date") %>%
    arrange(Date)
  
  
write.xlsx(df_final_BDF, file = "Final_Results/BDF_rolling_text_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #3

write.xlsx(df_final_INSEE, file = "Final_Results/INSEE_rolling_Text_2020.xlsx", 
           sheetName = 'prevision', rowNames = FALSE)  #4




