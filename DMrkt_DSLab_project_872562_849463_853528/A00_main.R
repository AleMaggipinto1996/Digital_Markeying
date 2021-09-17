#### OPTIONS ####

options(scipen=999)
set.seed(123456)

#### LIBRARIES ####
library(dplyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
library(tidyverse)
library(scales)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(plotly)
library(randomForest)
library(ipred)
library(rpart)
library(pROC)
library(caret)
library(rpart.plot)
library(MLmetrics)
library(e1071)
library(funModeling)

#### DIRECTORIES ####
working_dir = "/Users/alessandramaggipinto/Documents/GitHub/Progetto_DigitalM"
data_dir = "/Users/alessandramaggipinto/Documents/DMktg_DSLab_Data_1"
setwd(working_dir)

#### EXECUTION FULL PIPELINE ####

PIPELINE_scripts <- c(
'B01_ingestion.R'
, 'C01_preparation_df1.R'
, 'C02_preparation_df2.R'
, 'C03_preparation_df3.R'
, 'C04_preparation_df4.R'
, 'C05_preparation_df5.R'
, 'C06_preparation_df6.R'
, 'C07_preparation_df7.R'
, 'D01_preparation_df1_aziende.R'
, 'D02_preparation_df2_aziende.R'
, 'D03_preparation_df3_aziende.R'
, 'D04_preparation_df4_aziende.R'
, 'D06_preparation_df6_aziende.R'
, 'D07_preparation_df7_aziende.R'
, 'E01_RFM_persone.R'
, 'E02_RFM_aziende.R'
, 'F01_Churn_persone.R'
, 'F02_Churn_aziende.R'
, 'G01_MBA_Persone.R'
, 'G02_MBA_Aziende.R'
)
 
 for(i in PIPELINE_scripts){
 source(i, echo = TRUE)
 }

