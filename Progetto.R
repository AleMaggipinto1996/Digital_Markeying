### Importazione delle librerie

library(dplyr)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(pander)
library(tidyverse)
library(wesanderson)


### Importazione dei Dataset

raw_1_cli_fid <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_1_cli_fid.csv", sep=";", na.strings = c("NA", ""))

df_2_cli_account <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_2_cli_account.csv", sep=";", na.strings = c("NA", ""))

df_3_cli_address <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_3_cli_address.csv", sep=";", na.strings = c(""))

df_4_cli_privacy <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_4_cli_privacy.csv", sep=";", na.strings = c("NA", ""))

df_5_camp_cat <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_5_camp_cat.csv", sep=";", na.strings = c("NA", ""))

df_6_camp_event <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_6_camp_event.csv", sep=";", na.strings = c("NA", ""))

df_7_tic <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_7_tic.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)
