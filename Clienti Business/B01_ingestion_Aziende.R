### DATA PREPROCESSING

#-------------------------------


#### iMPORTAZIONE DEI Dataset ####

#informazioni carta fedelt√† di ciascun cliente
df_1_cli_fid <- read.csv2("C:/Users/Angelo/Desktop/Marketing e mercati globali/2o anno/2∞ semestre/Digital marketing/LAB -Progetto/datii/raw_1_cli_fid.csv", sep=";", na.strings = c("NA", ""))

#informazioni account cliente
df_2_cli_account <- read.csv2("C:/Users/Angelo/Desktop/Marketing e mercati globali/2o anno/2∞ semestre/Digital marketing/LAB -Progetto/datii/raw_2_cli_account.csv", sep=";", na.strings = c("NA", ""))

#indirizzo cliente
df_3_cli_address <- read.csv2("C:/Users/Angelo/Desktop/Marketing e mercati globali/2o anno/2∞ semestre/Digital marketing/LAB -Progetto/datii/raw_3_cli_address.csv", sep=";", na.strings = c(""))

#privacy clienti
df_4_cli_privacy <- read.csv2("C:/Users/Angelo/Desktop/Marketing e mercati globali/2o anno/2∞ semestre/Digital marketing/LAB -Progetto/datii/raw_4_cli_privacy.csv", sep=";", na.strings = c("NA", ""))

#email campaign
df_5_camp_cat <- read.csv2("C:/Users/Angelo/Desktop/Marketing e mercati globali/2o anno/2∞ semestre/Digital marketing/LAB -Progetto/datii/raw_5_camp_cat.csv", sep=";", na.strings = c("NA", ""))

#email events
df_6_camp_event <- read.csv2("C:/Users/Angelo/Desktop/Marketing e mercati globali/2o anno/2∞ semestre/Digital marketing/LAB -Progetto/datii/raw_6_camp_event.csv", sep=";", na.strings = c("NA", ""))

#operazioni acquisto/rimborso di ogni cliente
df_7_tic <- read.csv2("C:/Users/Angelo/Desktop/Marketing e mercati globali/2o anno/2∞ semestre/Digital marketing/LAB -Progetto/datii/raw_7_tic.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)



