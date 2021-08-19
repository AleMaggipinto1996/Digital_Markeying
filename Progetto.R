### Importazione delle librerie

library(dplyr)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(pander)
library(tidyverse)
library(wesanderson)


### Importazione dei Dataset

raw_1_cli_fid <- read.csv("~/Documents/GitHub/Progetto_DigitalM/DMktg_DSLab_Data_1/raw_1_cli_fid.csv", sep=";", na.strings = c("NA", ""))
df_2_cli_account <- read.csv("~/Documents/GitHub/Progetto_DigitalM/DMktg_DSLab_Data_1/raw_2_cli_account.csv", sep=";", na.strings = c("NA", ""))
df_3_cli_address <- read.csv("~/Documents/GitHub/Progetto_DigitalM/DMktg_DSLab_Data_1/raw_3_cli_address.csv", sep=";", na.strings = c(""))
df_4_cli_privacy <- read.csv("~/Documents/GitHub/Progetto_DigitalM/DMktg_DSLab_Data_1/raw_4_cli_privacy.csv", sep=";", na.strings = c("NA", ""))
