#### OPTIONS ####

options(scipen=999)
set.seed(123456)

#### LIBRARIES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(pander)
library(tidyverse)
library(wesanderson)
library(forcats)
library(lubridate)
library(RQuantLib)
library(rfm)
library(scales)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(plotly)
library(randomForest)
library(ipred)
library(rpart)
library(pROC)
library(forcats)
library(RQuantLib)
library(caret)
library(rpart.plot)
library(MLmetrics)
library(e1071)
library(funModeling)


#### DIRECTORIES ####
getwd()
working_dir = "C:/Users/Utente/Documents/GitHub/Progetto_DigitalM"
data_dir = "C:/Users/Utente/Desktop/Web_marketing/Data"
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
, 'C02_preparation_df2_aziende.R'
, 'C03_preparation_df3_aziende.R'
, 'C04_preparation_df4_aziende.R'
, 'C05_preparation_df5_aziende.R'
, 'C06_preparation_df6_aziende.R'
, 'C07_preparation_df7_aziende.R'
, 'MBA_persone.R'
, 'MBA_aziende.R'
, 'RFM.R'
, 'RFM_aziende.R'
, 'Churn.R'
, 'Churn_aziende.R'
)
 
 for(i in PIPELINE_scripts){
 source(i, echo = TRUE)
 }

#writeLines(capture.output(sessionInfo()), "sessionInfo.txt")