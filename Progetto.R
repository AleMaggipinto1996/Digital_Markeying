#----------------------------------------
####OPTIONS####

options(scipen = 999)
set.seed(123456)

####LIBRERIE####

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


library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(funModeling)
library(arules)
library(arulesViz)
library(tidyr)





#-------------------------------

### DATA PREPROCESSING

#-------------------------------


#### iMPORTAZIONE DEI Dataset ####

#informazioni carta fedeltà di ciascun cliente
df_1_cli_fid <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_1_cli_fid.csv", sep=";", na.strings = c("NA", ""))

#informazioni account cliente
df_2_cli_account <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_2_cli_account.csv", sep=";", na.strings = c("NA", ""))

#indirizzo cliente
df_3_cli_address <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_3_cli_address.csv", sep=";", na.strings = c(""))

#privacy clienti
df_4_cli_privacy <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_4_cli_privacy.csv", sep=";", na.strings = c("NA", ""))

#email campaign
df_5_camp_cat <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_5_camp_cat.csv", sep=";", na.strings = c("NA", ""))

#email events
df_6_camp_event <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_6_camp_event.csv", sep=";", na.strings = c("NA", ""))

#operazioni acquisto/rimborso di ogni cliente
df_7_tic <- read.csv2("~/Documents/DMktg_DSLab_Data_1/raw_7_tic.csv", na.strings = c("NA", ""), stringsAsFactors = FALSE)



#### PREPARAZIONE DEI DATASET ####

### Dataset n°1 ###

###primo sguardo al dataset

str(df_1_cli_fid)  #come sono fatti i nostri dati
summary(df_1_cli_fid)

#si vede che la maggior parte dei clienti hanno un account ed è attivo

## START CLEANING df_1 ##

### Ricreare il dataset

df_1_cli_fid_clean <- df_1_cli_fid

### Check for duplicates (non duplicati per CLI-FID)
df_1_cli_fid_clean %>% summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ID_FIDs = n_distinct(ID_FID)
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
            , TOT_ROWs = n())

#ci sono più registrazioni di carte fedeltà di ciascun cliente e questo non ci sorprende

### Formattazione delle date 
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

### Formattazione boleani in fattori ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

### Numero di programmi fedeltà per numero di clienti ##
##quante sottoscrizioni ho per ciascun cliente?

num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID)
            , NUM_DATEs = n_distinct(DT_ACTIVE)
  )

#ci sono clienti che vedendo la data hanno probabilmente sbagliato/cambiato la fidelizzazione più volte
#nello stesso giorno

tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)

## compute the distribution of number of subscriptions
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

## Ci sono clienti con molteplici programmi fedeltà ##
# let examine in details clients with multiple subscriptions#

num_fid_x_cli %>% filter(NUM_FIDs == 3)

#andiamo a vedere nel dettaglio per esempio un cliente#
# each subscription can have different dates
df_1_cli_fid %>% filter(ID_CLI == 621814)
# there could be subscriptions at the same dates [possibly for technical reasons]
df_1_cli_fid %>% filter(ID_CLI == 320880)

#### RESHAPING df_1 ####

## combining information ##

# from first subscription  --> registration date, store for registration
# from last subscription   --> type of fidelity, status
# from subscriptions count --> number of subscriptions made

#ID 1 è l'ID relativo alla registrazione online, gli altri sono negozi


df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

#ricombinare le informazioni per avere un dataset completo

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

### variabile Registrazione Online / Negozio Fisico ###

#creo una nuova colonna 0/1: è 1 se si tratta di registrazione online, 
#è 0 se è stata fatta nel negozio

RegOnline <- as.data.frame(df_1_cli_fid_clean$FIRST_ID_NEG)
colnames(RegOnline)<- "RegOnline"

RegOnline <- RegOnline %>% mutate(RegOnline = as.factor(if_else(RegOnline!= 1, "0", as.character(RegOnline))))

df_1_cli_fid_clean$RegOnline <- RegOnline$RegOnline



## EXPLORE COLUMNS of df_1 ##

### variable LAST_COD_FID ###

## come si distribuiscono i nostri clienti
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## plot distribution
plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_COD_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

#si vede una sproporzione elevata tra quelli che hanno
#una fidelizzazione standard e quelli premium

#biz vuol dire categorizzazione tra clienti business e non
#ovvero clienti e aziende vere e proprie

#è importante capire il modo migliore per presentare i dati

##### SEPARIAMO IN DUE DATASET AZIENDE/PERSONE ####

df_1_aziende <- df_1_cli_fid_clean %>%  
  filter(LAST_COD_FID == 'PREMIUM BIZ' | LAST_COD_FID == 'STANDARD BIZ')

df_1_persone <- df_1_cli_fid_clean %>%  
  filter(LAST_COD_FID == 'PREMIUM' | LAST_COD_FID == 'STANDARD')

#_________________________________________

############# consideriamo la parte che riguarda le persone fisiche ###########à

#_________________________________________


### variable LAST_DT_ACTIVE per MESI ###

## compute distribution 
df1_persone_codfid_ld <- df_1_persone %>%
  group_by(substring(LAST_DT_ACTIVE,1,7)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Mesi = `substring(LAST_DT_ACTIVE, 1, 7)`)

df1_persone_codfid_ld


## plot distribution

plot_df1_persone_codfid_ld <- (
  ggplot(data=df1_persone_codfid_ld
         , aes(x=Mesi, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_persone_codfid_ld


### variable LAST_DT_ACTIVE per ANNI ###

## compute distribution 
df1_p_codfid_ld <- df_1_persone %>%
  group_by(substring(LAST_DT_ACTIVE,1,4)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Year = `substring(LAST_DT_ACTIVE, 1, 4)`)

df1_p_codfid_ld


## plot distribution

plot_df1_p_codfid_ld <- (
  ggplot(data=df1_p_codfid_ld
         , aes(x=Year, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_p_codfid_ld


### variable FIRST_DT_ACTIVE per MESI ###

## compute distribution
df1_p_codfid_fd <- df_1_persone %>%
  group_by(substring(FIRST_DT_ACTIVE,1,7)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Mesi = `substring(FIRST_DT_ACTIVE, 1, 7)`)

df1_p_codfid_fd

## plot distribution

plot_df1_p_codfid_fd <- (
  ggplot(data=df1_p_codfid_fd
         , aes(x=Mesi, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_p_codfid_fd



### variable FIRST_DT_ACTIVE per ANNI ###

## compute distribution
df1_p_codfid_fd <- df_1_persone %>%
  group_by(substring(FIRST_DT_ACTIVE,1,4)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Year = `substring(FIRST_DT_ACTIVE, 1, 4)`)

df1_p_codfid_fd

## plot distribution

plot_df1_p_codfid_fd <- (
  ggplot(data=df1_p_codfid_fd
         , aes(x=Year, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid_fd


### variable LAST_STATUS_FID ###

## compute distribution
df1_p_codfid_status <- df_1_persone %>%
  group_by(LAST_STATUS_FID) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))


df1_p_codfid_status

## plot distribution

plot_df1_p_codfid_status <- (
  ggplot(data=df1_dist_p_status
         , aes(x=LAST_STATUS_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_p_codfid_status


### variable NUM_FIDS ###

## compute distribution
df1_p_codfid_n <- df_1_persone%>%
  group_by(NUM_FIDs) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_p_codfid_n

## plot distribution

plot_df1_p_codfid_n <- (
  ggplot(data=df1_p_codfid_n
         , aes(x=NUM_FIDs, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_p_codfid_n

### variable LAST_TYP_CLI_FID ???? non abbiamo capito che vuol dire ###

## compute distribution
df1_p_codfid_main <- df_1_persone %>%
  group_by(LAST_TYP_CLI_FID) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_p_codfid_main

## plot distribution

plot_df1_p_codfid_main <- (
  ggplot(data=df1_p_codfid_main
         , aes(x=LAST_TYP_CLI_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_p_codfid_main


### variabile Negozio Online / Negozio Fisico ###

## compute distribution
df1_p_codfid_neg <- df_1_cli_fid_clean %>%
  group_by(RegOnline) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_p_codfid_neg

## plot distribution

plot_df1_p_codfid_neg <- (
  ggplot(data=df1_p_codfid_neg
         , aes(x=FIRST_ID_NEG, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_p_codfid_neg


#### FINAL REVIEW df_1_clean ####

str(df_1_persone)
summary(df_1_persone)

#________________________________________________

#### FIRST LOOK of df_2 ####

str(df_2_cli_account)
summary(df_2_cli_account)

#ci sono persone che hanno aggiunto telefoni e poi NA,
#che bisognerebbe convertire in 0



#### START CLEANING df_2 ####

df_2_cli_account_clean <- df_2_cli_account

## check for duplicates
df_2_cli_account_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

## format boolean as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

## format numerical categories as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

#### CLEANING MISSING VALUES in df_2 ####

## MISSING VALUES mapped as natural values ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0"))

## MISSING VALUES mapped as new level in categorical columns ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%  
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))

#### CONSISTENCY CHECK ID_CLI in df_1/df_2 ####

cons_idcli_df1_df2 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_cli_account_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2

#!!! NOTE: all ID_CLI in df_1 are also in df_2 and vice-versa !!!#

#### divido dataset tra persone e aziende ####

df_2_aziende <- df_2_cli_account_clean %>%  
  filter(TYP_CLI_ACCOUNT == 2)

df_2_persone <- df_2_cli_account_clean %>%  
  filter(TYP_CLI_ACCOUNT == 4)


cons_idcli_df1_df2 <- df_1_persone %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_persone %>%
              select(ID_CLI) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2

#ci sono 22 casi che nel df2 sono registrati come persone e nel df1 come aziende
# df2 333656 invece df1 333634

## come gestiamo questi 22 casi? ci sarà un errore, gli eliminiamo?


#### EXPLORE COLUMNS of df_2_ PERSONE ####

### Variable EMAIL_PROVIDER ###

## compute distribution
df_2_p_emailprovider <- df_2_persone %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_p_emailprovider

tot_emailproviders <- n_distinct(df_2_p_emailprovider$EMAIL_PROVIDER)

tot_emailproviders

#!!! NOTE: too many different values for EMAIL_PROVIDER to be an useful category !!!#


#### EMAIL_PROVIDER ####
#______________________________________
##mantieni i valori EMAIL_PROVIDER più frequenti e 
##aggiungi un livello di fattore comune "ALTRO" per i restanti

df_2_p_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  as.data.frame() %>%
  head(20)

## always keep the (missing) level for technical reasons
## select levels that cover the 85% of the cases, the remaining 15% 
clean_email_providers <- df_2_p_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

## add clean EMAIL_PROVIDER ##
df_2_persone <- df_2_persone %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))


#### EXPLORE NEW COLUMNS EMAIL_PROVIDER_CLEAN in df_2 ####

## compute distribution
df2_p_emailproviderclean <- df_2_persone %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_p_emailproviderclean

## plot distribution
plot_df2_p_emailproviderclean <- (
  ggplot(data=df2_p_emailproviderclean
         , aes(x=EMAIL_PROVIDER_CLEAN, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df2_p_emailproviderclean
#è usata soprattutto @gmail

#_____________________________________________________

# EXPLORE the remaining relevant variables


### variabile occupazione cliente ###

df_2_p_typJob <- df_2_persone %>%
  group_by(TYP_JOB) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  as.data.frame()

df_2_p_typJob

# la maggior parte della colonna ha dati mancanti
# uniamo non dichiarati con quelli mancanti

df_2_typJob_miss <- df_2_p_typJob %>% 
  filter(TYP_JOB == "(missing)")

df_2_typJob_nondic <- df_2_p_typJob %>% 
  filter(TYP_JOB == "Non Dichiara") %>%
  mutate(TOT_CLIs = TOT_CLIs + df_2_typJob_miss[, "TOT_CLIs"])

df_2_p_typJob <- df_2_p_typJob %>% 
  filter(TYP_JOB != "Non Dichiara" & TYP_JOB != "(missing)") %>%
  bind_rows(df_2_typJob_nondic) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs))

df_2_p_typJob

n_cli = sum((df_2_p_typJob %>% filter(TYP_JOB != "Non Dichiara"))$TOT_CLIs)

#mostriamo l'occupazione dei clienti togliendo la percentuale di quelli non dichiarati

plot_df_2_p_typJob <- df_2_p_typJob %>%
  filter(TYP_JOB != "Non Dichiara") %>%
  group_by(TYP_JOB) %>%
  dplyr::summarize(PERCENT = TOT_CLIs / n_cli) %>%
  ggplot(aes(PERCENT, TYP_JOB)) +
  ggtitle("Occupazione clienti") +
  geom_bar(stat = "identity", fill = "#005c99", color = "#005c99") +
  xlab("Percentuale clienti") +
  ylab("Occupazione clienti") +
  scale_x_continuous(breaks = seq(0,1,0.1), labels = scales::percent_format(scale = 100)) +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(axis.text = element_text(size = 10, face = "italic")) +
  theme(axis.title = element_text(size = 13))

plot_df_2_p_typJob




### variabile W_PHONE ###

## compute distribution
df_2_p_phone <- df_2_persone %>%
  group_by(W_PHONE) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_p_phone

## plot distribution

plot_df_2_p_phone <- (
  ggplot(data=df_2_p_phone
         , aes(x=W_PHONE, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_2_p_phone


### variabile TYP_ACCOUNT ###

## compute distribution
df_2_p_type <- df_2_persone %>%
  group_by(TYP_CLI_ACCOUNT) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_p_type

## plot distribution

plot_df_2_p_type <- (
  ggplot(data=df_2_p_type
         , aes(x=TYP_CLI_ACCOUNT, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_2_cli_type

#### FINAL REVIEW df_2_clean ####

str(df_2_persone)
summary(df_2_persone)





#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarize(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

#!!! NOTE:  there are duplicates !!!#

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()


#### CLEANING DATA TYPES in df_3 ####

## format string as factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))

#### CLEANING MISSING VALUES in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))

## let examine in details some of these missing cases
df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarize(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3


#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#

#### considerimo le persone utilizzando il dataset df_2_persone ####

id_persone_indirizzo <- as.data.frame(df_2_persone$ID_ADDRESS)
colnames(id_persone_indirizzo) <- "ID_ADDRESS"

df_3_persone <- merge( df_3_cli_address_clean, id_persone_indirizzo, by="ID_ADDRESS")

id_aziende_indirizzo <- as.data.frame(df_2_aziende$ID_ADDRESS)
colnames(id_aziende_indirizzo) <- "ID_ADDRESS"

df_3_aziende <- merge( df_3_cli_address_clean, id_aziende_indirizzo, by="ID_ADDRESS")

#### EXPLORE COLUMNS of df_3 ####

# EXPLORE the df_3_cli_address_clean relevant variables

### Variabile REGION ###

df_3_persone_region_distrib <- df_3_persone %>%
  group_by(REGION) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_persone_region_distrib

## plot distribution
plot_df_3_persone_region_distrib <- (
  ggplot(data=df_3_persone_region_distrib
         , aes(x=REGION, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_persone_region_distrib

#soprattutto la Lombardia

### variabile PROVINCIA ###

## compute distribution
df_3_persone_prv_distrib <- df_3_persone %>%
  group_by(PRV) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_persone_prv_distrib

## plot distribution
plot_df_3_persone_prv_distrib <- (
  ggplot(data=df_3_persone_prv_distrib
         , aes(x=PRV, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_persone_prv_distrib

# troppe variabili non è significativo

### variabile CAP ###

## compute distribution
df_3_persone_cap_distrib <- df_3_persone %>%
  group_by(CAP) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_persone_cap_distrib

#anche i CAP ci sono troppe variabili non è utile

#### FINAL REVIEW df_3_clean ####

str(df_3_persone)
summary(df_3_persone)




#### FIRST LOOK of df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting boolean as factor ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

##### consideriamo solo la categoria persone #####

id_persone <- as.data.frame(df_1_persone$ID_CLI)
colnames(id_persone) <- "ID_CLI"

df4_persone <- merge(id_persone, df_4_cli_privacy_clean, by="ID_CLI")

id_aziende <- as.data.frame(df_1_aziende$ID_CLI)
colnames(id_aziende) <- "ID_CLI"

df_4_aziende <- merge(id_aziende, df_4_cli_privacy_clean, by="ID_CLI")

#### EXPLORE COLUMNS of df_4 ####

# EXPLORE the df_4_cli_privacy_clean relevant variables

### variabile Privacy 1 ###

## compute distribution
df_4_p_flag1_distrib <- df4_persone %>%
  group_by(FLAG_PRIVACY_1) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_p_flag1_distrib

## plot distribution
plot_df_4_p_flag1_distrib <- (
  ggplot(data=df_4_p_flag1_distrib
         , aes(x=FLAG_PRIVACY_1, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_p_flag1_distrib

### Variabile Privacy 2 ###

## compute distribution
df_4_cli_p_flag2_distrib <- df4_persone %>%
  group_by(FLAG_PRIVACY_2) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_cli_p_flag2_distrib

## plot distribution
plot_df_4_p_flag2_distrib <- (
  ggplot(data=df_4_p_flag2_distrib
         , aes(x=FLAG_PRIVACY_2, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_p_flag2_distrib

### variabile direct_marketing ###

## compute distribution
df_4_p_flag_mkt_distrib <- df4_persone %>%
  group_by(FLAG_DIRECT_MKT) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_p_flag_mkt_distrib

## plot distribution
plot_df_4_p_flag_mkt_distrib <- (
  ggplot(data=df_4_p_flag_mkt_distrib
         , aes(x=FLAG_DIRECT_MKT, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_p_flag_mkt_distrib


#### FINAL REVIEW df_4_clean ####

str(df4_persone)
summary(df4_persone)






#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

## variabile TYP_CAMP ##

## compute distribution
df_5_camp_cat_type_camp <- df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_5_camp_cat_type_camp



#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)




#### FIRST LOOK of df_6 ####

str(df_6_camp_event)
summary(df_6_camp_event)

#### START CLEANING df_6 ####

df_6_camp_event_clean <- df_6_camp_event

#### CLEANING DATA TYPES in df_6 ####

## formatting dates and times ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATETIME = as.POSIXct(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S")) %>%
  mutate(EVENT_HOUR = hour(EVENT_DATETIME)) %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATETIME))

#### CONSISTENCY CHECK ID_CLI in df_1/df_6 ####

cons_idcli_df1_df6 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_6) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df6

#!!! NOTE: all ID_CLI in df_6 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_6 !!!#  

#### CONSISTENCY CHECK ID_CAMP in df_5/df_6 ####

cons_idcamp_df5_df6 <- df_5_camp_cat_clean %>%
  select(ID_CAMP) %>%
  distinct() %>%
  mutate(is_in_df_5 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CAMP) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CAMP"
  ) %>%
  group_by(is_in_df_5, is_in_df_6) %>%
  summarize(NUM_ID_CAMPs = n_distinct(ID_CAMP)) %>%
  as.data.frame()

cons_idcamp_df5_df6

#!!! NOTE: all ID_CAMP in df_6 are mapped in df_5, but not all ID_CAMP in df_5 are mapped in df_6 !!!#

#### RESHAPING df_6 ####

## remapping TYPE_EVENT values "E" [ERROR] and "B" [BOUNCE] into a level "F" [FAILURE] ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))

## adding type from df_5 ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")

## organize the data adding to each sending event the corresponding opens/clicks/fails

# sends
df_sends <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE = EVENT_DATE) %>%
  as.data.frame()

# opens
# there could be multiple opens of the same communication
# 1- count the open events
# 2- consider explicitely only the first open

df_opens_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , OPEN_DATETIME = EVENT_DATETIME
         , OPEN_DATE = EVENT_DATE)

total_opens <- df_opens_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarize(NUM_OPENs = n_distinct(ID_EVENT_O))

df_opens <- df_opens_prep %>%
  left_join(total_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(OPEN_DATETIME == min(OPEN_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# clicks
# there could be multiple clicks of the same communication
# 1- count the click events
# 2- consider explicitely only the first click

df_clicks_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , CLICK_DATETIME = EVENT_DATETIME
         , CLICK_DATE = EVENT_DATE)

total_clicks <- df_clicks_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarize(NUM_CLICKs = n_distinct(ID_EVENT_C))

df_clicks <- df_clicks_prep %>%
  left_join(total_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(CLICK_DATETIME == min(CLICK_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# fails
df_fails <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , FAIL_DATETIME = EVENT_DATETIME
         , FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATETIME == min(FAIL_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# combine sends opens clicks and fails
df_6_camp_event_clean_final <- df_sends %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | OPEN_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(OPENED = !is.na(ID_EVENT_O)) %>%
  mutate(CLICKED = !is.na(ID_EVENT_C)) %>%
  mutate(FAILED = !is.na(ID_EVENT_F)) %>%
  mutate(DAYS_TO_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  select(ID_EVENT_S
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE
         
         , OPENED
         , OPEN_DATE
         , DAYS_TO_OPEN
         , NUM_OPENs
         
         , CLICKED
         , CLICK_DATE
         , NUM_CLICKs
         
         , FAILED
  )

##### consideriamo la categoria persone ######

df_6_persone <- merge( id_persone,df_6_camp_event_clean_final, by="ID_CLI")

df_6_aziende <- merge(id_aziende, df_6_camp_event_clean_final, by="ID_CLI")

cons_idcli_df1_df6 <- df_1_persone %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_6_persone %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_6) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df6

#### anche qui tutte quelle in df_6 sono mappate in df_1, ma non tutte quelle di df_1 sono in df_6

#### EXPLORE VARIABLES in df_6 ####

### GENERAL OVERVIEW ###

## compute aggregate
df6_overview <- df_6_persone %>% 
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overview

### GENERAL OVERVIEW by TYP_CAMP ###

## compute aggregate
df6_overviewbytyp <- df_6_persone %>%
  group_by(TYP_CAMP) %>%
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overviewbytyp

## plot aggregate
plot_df6_overviewbytyp <- (
  ggplot(data=df6_overviewbytyp
         , aes(x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_overviewbytyp

###sono state fatte soprattutto campagne nazionali

### Variable OPENED ###

## compute aggregate
df6_dist_opened <- df_6_persone %>%
  group_by(OPENED) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_opened

## plot aggregate
plot_df6_dist_opened <- (
  ggplot(data=df6_dist_opened
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    theme_minimal()
)

plot_df6_dist_opened

### per la maggior parte non sono state aperte

### Variable OPENED by TYP_CAMP ###

## compute aggregate
df6_dist_openedbytyp <- df_6_persone %>%
  group_by(TYP_CAMP, OPENED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , OPENED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_openedbytyp

## plot aggregate
plot_df6_dist_openedbytyp <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_dist_openedbytyp

## plot aggregate percent
plot_df6_dist_openedbytyp_percent <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_dist_openedbytyp_percent

#### sono state aperte di più quelle riferite al prodotto, poi
### quelle personalizzare, quelle locali e infine quelle nazionali

### Variable DAYS_TO_OPEN

## compute aggregate
df6_dist_daystoopen <- df_6_persone %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarize(AVG_DAYS_TO_OPEN = floor(mean(DAYS_TO_OPEN))) %>%
  ungroup() %>%
  group_by(AVG_DAYS_TO_OPEN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_daystoopen

## plot aggregate
plot_df6_dist_daystoopen <- (
  ggplot(data=df6_dist_daystoopen %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_dist_daystoopen

#### la maggior parte vengono aperte subito o il giorno dopo
##più passano i giorni più diminuiscono

### DAYS_TO_OPEN vs CUMULATE PERCENT ### non ho capito cos'è ???????

## compute aggregate
df6_dist_daystoopen_vs_cumulate <- df6_dist_daystoopen %>%
  arrange(AVG_DAYS_TO_OPEN) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

## plot aggregate
plot_df6_dist_daystoopen_vs_cumulate <- (
  ggplot(data=df6_dist_daystoopen_vs_cumulate %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=PERCENT_COVERED)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) +
    theme_minimal()
)

plot_df6_dist_daystoopen_vs_cumulate

# EXPLORE the following relevant variables in df_6_camp_event_clean_final:

# - CLICKED/CLICKED by TYP_CAMP

df6_dist_clickedbytyp <- df_6_persone %>%
  group_by(TYP_CAMP, CLICKED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
                   , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , CLICKED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_clickedbytyp

## plot aggregate
plot_df6_dist_clickedbytyp <- (
  ggplot(data=df6_dist_clickedbytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_dist_clickedbytyp

## plot aggregate percent
plot_df6_dist_clickedbytyp_percent <- (
  ggplot(data=df6_dist_clickedbytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_dist_clickedbytyp_percent

# in generale pochissimi click
#più sui prodotti e su quelli nazionali


# - FAILED/FAILED by TYP_CAP

## compute aggregate
df6_dist_failedbytyp <- df_6_persone %>%
  group_by(TYP_CAMP, FAILED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
                   , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , FAILED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_failedbytyp

## plot aggregate
plot_df6_dist_failedbytyp <- (
  ggplot(data=df6_dist_failedbytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_dist_failedbytyp

## plot aggregate percent
plot_df6_dist_failedbytyp_percent <- (
  ggplot(data=df6_dist_failedbytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_dist_failedbytyp_percent

# pochi eventi falliti, ma soprattutto quelli nazionali

# - NUM_OPENs

# compute aggregate
df6_dist_num_opens <- df_6_persone %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarize(AVG_OPENs = floor(mean(NUM_OPENs))) %>%
  ungroup() %>%
  group_by(AVG_OPENs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_num_opens 

## plot 
plot_df6_dist_num_opens <- (
  ggplot(data=df6_dist_num_opens
         , aes(x=AVG_OPENs, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_dist_num_opens

#pochi clienti riaprono più di una volta, molto pochi più di due

# - NUM_CLICKs

# compute aggregate
df6_num_clicks <- df_6_persone %>%
  filter(CLICKED) %>%
  group_by(ID_CLI) %>%
  summarize(AVG_CLICKs = floor(sum(NUM_CLICKs))) %>%
  ungroup() 
df6_dist_num_clicks <- df6_num_clicks %>%
  group_by(AVG_CLICKs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_num_clicks

## plot 
plot_df6_dist_num_clicks <- (
  ggplot(data=df6_dist_num_clicks
         , aes(x=AVG_CLICKs, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_dist_num_clicks

# per i click sono stati cliccati diverse volte, ma principalemnte sempre una volta


#### FINAL REVIEW df_6_clean ####

str(df_6_persone)
summary(df_6_persone)



#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )

## aggiunto se è weekend, festivo, giorno della settimana, altro

df_7_persone <- merge( id_persone,df_7_tic_clean_final, by="ID_CLI")

df_7_aziende <- merge( id_aziende,df_7_tic_clean_final, by="ID_CLI")

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

### ugualmente alcuni clienti mappati in df_1 non lo sono in df_7

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_persone %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_persone %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction


### molta merce venduta poca rimborsata

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_persone %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_persone %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_dep

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_persone %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_datetyp

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

## gli acquisti vengono fatti soprattutto nei giorni lavorativi poi nel weekend e poco nei giorni festivi
## forse perchè essendo negozi fisici sono chiusi. I rimborsi si suddividono in maniera equa

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###
#capire l'utilità e come sono stati scritti ?????

## compute aggregate
df7_dist_importosconto <- df_7_persone %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO

#capire l'utilità e come sono stati scritti ?????


## compute aggregate
df7_dist_importosconto_reparto <- df_7_persone %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  dplyr::summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
                   , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_reparto <- df7_dist_importosconto_reparto %>%
  group_by(DIREZIONE) %>%
  dplyr::summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
                   , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_reparto

## plot aggregate
plot_df7_dist_importo_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto %>%
           filter()
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo_reparto

## plot aggregate
plot_df7_dist_sconto_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto %>%
           filter()
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto_reparto


# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

# number of tics per articolo
df7_dist_tics_articolo <- df_7_persone %>%
  group_by(ID_ARTICOLO) %>%
  dplyr::summarize(NUM_TICs = sum(n_distinct(ID_SCONTRINO))) %>%
  ungroup()

df7_dist_tics_articolo

# distribution of TICs number    ## quante volte sono stati acquistati tot volte articoli diversi
df7_dist_numtics_articolo <- df7_dist_tics_articolo %>%
  group_by(NUM_TICs) %>%
  dplyr::summarize(COUNT_ART = sum(n_distinct(ID_ARTICOLO))) %>%
  ungroup()

df7_dist_numtics_articolo

# plot aggregate
plot_df7_dist_numtics_articolo <- df7_dist_numtics_articolo %>%
  filter(NUM_TICs < 50) %>%
  ggplot(aes(x = NUM_TICs, y = COUNT_ART)) +
  geom_histogram(stat = "identity", fill = "#549900") + 
  ggtitle("Distribution of Numb TICs by ID_ARTICOLO") + 
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  xlab("Numb of Articles") +
  ylab("Numb of transactions") +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(axis.text = element_text(size = 10, face = "italic")) +
  theme(axis.title = element_text(size = 13))

plot_df7_dist_numtics_articolo

# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

## compute aggregate
df7_dist_importosconto_cli <- df_7_persone %>%
  group_by(ID_CLI, DIREZIONE) %>%
  dplyr::summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
                   , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_cli <- df7_dist_importosconto_cli %>%
  group_by(DIREZIONE) %>%
  dplyr::summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
                   , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_cli

## plot aggregate
plot_df7_dist_importo_cli <- (
  ggplot(data=df7_dist_importosconto_cli %>%
           filter()
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo_cli

## plot aggregate
plot_df7_dist_sconto_cli <- (
  ggplot(data=df7_dist_importosconto_cli %>%
           filter()
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto_cli


# compute the distribution of customers by number of purchases (as described in the slides)

df7_dist_total_purch <- df_7_persone %>%
  filter(DIREZIONE == 1)                             %>% 
  group_by(ID_CLI)                                   %>% 
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>% 
  arrange(desc(TOT_PURCHASE))                           

df7_dist_total_purch


# compute the days for next purchase curve (as described in the slides)

df_for_next_purchase_curve <- df_7_persone %>%
  filter(DIREZIONE == 1) %>% 
  select(ID_CLI,
         ID_ARTICOLO,
         TIC_DATE,
         DIREZIONE)      %>%
  arrange(ID_CLI)

df_for_next_purchase_curve




#### FINAL REVIEW df_7_clean ####

str(df_7_persone)
summary(df_7_persone)





#################

# MODELLO RFM per la categoria persone

################

lastdate = df_7_persone %>% summarise(max(TIC_DATE))
lastdate=lastdate[1,1]
## prima data di acquisto risale al 2018-05-01, l'ultima data di acquisto risale al 2019-04-30

#vogliamo trovare i clienti attivi (80%)

#troviamo l'ultimo giorno di acquisto per ogni cliente
#e quanti giorni sono passati dall'ultimo acquisto in generale

df_last_date <- df_7_persone %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date)
boxplot(df_last_date$DIFF_DAYS)

giorni_max=quantile(df_last_date$DIFF_DAYS, probs = c(0.80, 0.85, 0.90))
giorni_max

#decidiamo di considerare attivi l'ottanta percento dei clienti

giorni_max = giorni_max[1]

clienti_attivi <- df_last_date %>% filter(DIFF_DAYS<=giorni_max)

df_clienti_attivi <- merge( df_7_persone, clienti_attivi, by="ID_CLI")

################### MODELLO RFM DAL 2018/08/01 AL 2019/01/31 #######################

df_clienti_attivi1 <- df_clienti_attivi %>% 
  filter(TIC_DATE <= as.Date("2019-01-31") & TIC_DATE >= as.Date("2018-08-01")) %>%
  select(-LAST_DATE_PURCH, - DIFF_DAYS)

df_last_date_persone_1 <- df_clienti_attivi1 %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_persone_1)
boxplot(df_last_date_persone_1$DIFF_DAYS)


df_clienti_attivi_1 <- merge(df_clienti_attivi1, df_last_date_persone_1, by="ID_CLI")

##Calcolo Recency: l'ultimo acquisto dopo quanto tempo?

quantili<- quantile(df_last_date_persone_1$DIFF_DAYS, probs = c(0.25, 0.50, 0.75))
quantili
#25% 50% 75% 
#109 143 181 

Recency_persone_1 = df_last_date_persone_1 %>% mutate(CLASS_R=case_when(DIFF_DAYS<quantili[1] ~ "Low",
                                                      (DIFF_DAYS>=quantili[1]) & (DIFF_DAYS<quantili[3])~"Medium",
                                                      (DIFF_DAYS>=quantili[3])~"High"))


Recency_persone_1 = mutate(Recency_persone_1,CLASS_R=factor(CLASS_R,levels=c("Low","Medium","High")))


##Calcolo Frequency: ogni quanto acquisto?

Frequency_persone_1 <- df_clienti_attivi_1 %>%
  filter(DIREZIONE==1) %>% group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>0)

boxplot(Frequency_persone_1$N_SCONTRINI_PER_CLI)

quantili<- quantile(Frequency_persone_1$N_SCONTRINI_PER_CLI,probs = c(0.50,0.70,0.90))
quantili
#50% 70% 90% 
#2   4   8 

Frequency_persone_1 <- Frequency_persone_1 %>% mutate(CLASS_F=case_when(N_SCONTRINI_PER_CLI<quantili[1] ~ "Low",
                                                    (N_SCONTRINI_PER_CLI>=quantili[1]) & (N_SCONTRINI_PER_CLI<quantili[3])~"Medium",
                                                    (N_SCONTRINI_PER_CLI>quantili[3])~"High"))
Frequency_persone_1 = mutate(Frequency_persone_1,CLASS_F=factor(CLASS_F,levels=c("Low","Medium","High")))

#Monetary Value (differenza tra importo lordo e sconto)

Monetary_Value_persone_1 = df_clienti_attivi_1 %>% filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(AMOUNT = sum(IMPORTO_LORDO) - sum(SCONTO))

quantili = quantile(Monetary_Value_persone_1$AMOUNT,probs = c(0.25,0.50,0.75))
quantili
#     25%      50%      75% 
#56.5900 163.8900 464.8075 

Monetary_Value_persone_1 = Monetary_Value_persone_1 %>% mutate(CLASS_M = case_when(AMOUNT<quantili[1]~"Low",
                                                               (AMOUNT>=quantili[1])&(AMOUNT<quantili[3])~"Medium",
                                                               AMOUNT>quantili[3]~"High"))
Monetary_Value_persone_1 = mutate(Monetary_Value_persone_1,CLASS_M=factor(CLASS_M,levels=c("Low","Medium","High")))

# Recency-Frequency

RF_persone_1 <- merge(Recency_persone_1, Frequency_persone_1,by="ID_CLI")
RF_persone_1 <- RF_persone_1 %>% mutate(CLASS_RF_persone_1 = case_when((CLASS_F=="Low")&(CLASS_R=="Low")~"One-Timer",
                                         (CLASS_F=="Low")&(CLASS_R=="Medium")~"One-Timer",
                                         (CLASS_F=="Low")&(CLASS_R=="High")~"Leaving",
                                         (CLASS_F=="Medium")&(CLASS_R=="Low")~"Engaged",
                                         (CLASS_F=="Medium")&(CLASS_R=="Medium")~"Engaged",
                                         (CLASS_F=="Medium")&(CLASS_R=="High")~"Leaving",
                                         (CLASS_F=="High")&(CLASS_R=="Low")~"Top",
                                         (CLASS_F=="High")&(CLASS_R=="Medium")~"Top",
                                         (CLASS_F=="High")&(CLASS_R=="High")~"Leaving Top"))

Fedelta_persone_1 <- as.data.frame(with(RF_persone_1,table(CLASS_RF_persone_1)))
Fedelta_persone_1 <- Fedelta_persone_1 %>% mutate(CLASS_RF_persone_1 = factor(CLASS_RF_persone_1,levels = c("Leaving","One-Timer",
                                                                       "Engaged","Leaving Top",
                                                                       "Top")))
Fedelta_persone_1
summary(Fedelta_persone_1)

#per la maggior parte sono Engaged e Leaving

Fedelta_persone_1$Freq <- (Fedelta_persone_1$Freq / nrow(RF_persone_1))

Fedelta_persone_1_plot <- 
  ggplot(Fedelta_persone_1,aes(CLASS_RF_persone_1,Freq,fill=CLASS_RF_persone_1)) + geom_bar(stat = "identity",width = 0.5)+
  ggtitle("Fedeltà")+ylab("Percentuale di Clienti")+xlab("Fedeltà")+
  scale_fill_brewer(palette = "Reds")+ scale_y_continuous(labels = percent) +
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

Fedelta_persone_1_plot

################### rendere più carino !!!!!!!!!!!!!!

##### RFM_persone_1

RFM_persone_1 <- RF_persone_1 %>% select(-c("LAST_DATE_PURCH","CLASS_R","CLASS_F")) %>% 
  left_join(Monetary_Value_persone_1,by="ID_CLI")
RFM_persone_1 <- mutate(RFM_persone_1,CLASS_RF_persone_1=factor(CLASS_RF_persone_1))


RFM_persone_1 <- RFM_persone_1 %>% mutate(CLASSI_persone_1 = case_when((CLASS_M=="Low") & (CLASS_RF_persone_1=="One-Timer")~"Cheap",
                                         (CLASS_M=="Low") & (CLASS_RF_persone_1=="Leaving")~"Tin",
                                         (CLASS_M=="Low") & (CLASS_RF_persone_1=="Engaged")~"Copper",
                                         (CLASS_M=="Low") & (CLASS_RF_persone_1=="Leaving Top")~"Bronze",
                                         (CLASS_M=="Low") & (CLASS_RF_persone_1=="Top")~"Silver",
                                         (CLASS_M=="Medium") & (CLASS_RF_persone_1=="One-Timer")~"Tin",
                                         (CLASS_M=="Medium") & (CLASS_RF_persone_1=="Leaving")~"Copper",
                                         (CLASS_M=="Medium") & (CLASS_RF_persone_1=="Engaged")~"Bronze",
                                         (CLASS_M=="Medium") & (CLASS_RF_persone_1=="Leaving Top")~"Silver",
                                         (CLASS_M=="Medium") & (CLASS_RF_persone_1=="Top")~"Gold",
                                         (CLASS_M=="High") & (CLASS_RF_persone_1=="One-Timer")~"Copper",
                                         (CLASS_M=="High") & (CLASS_RF_persone_1=="Leaving")~"Bronze",
                                         (CLASS_M=="High") & (CLASS_RF_persone_1=="Engaged")~"Silver",
                                         (CLASS_M=="High") & (CLASS_RF_persone_1=="Leaving Top")~"Gold",
                                         (CLASS_M=="High") & (CLASS_RF_persone_1=="Top")~"Diamond"))
RFM_persone_1 = RFM_persone_1 %>% mutate(CLASSI_persone_1 = factor(CLASSI_persone_1,levels = c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")))
RFM_TOT_persone_1 <- as.data.frame(with(RFM_persone_1,table(CLASSI_persone_1)))

RFM_TOT_persone_1 #la maggior parte dei clienti appartengono alla categoria Bronze, però troppi
         #clienti appartengono alla categoria cheap e tin.

RFM_TOT_persone_1$Freq <- (RFM_TOT_persone_1$Freq / nrow(RFM_persone_1))


#EXPLORATORY ANALYSIS of RFM's dataframe
RFM_persone_1_plot <- 
  ggplot(RFM_TOT_persone_1,aes(CLASSI_persone_1,Freq,fill=CLASSI_persone_1)) + geom_bar(stat = "identity")+
  labs(title = "Customer's distribution",size=18)+ylab("Percentuale clienti")+ scale_y_continuous(labels = percent)+
  scale_fill_manual(values=c("black","#2F4F4F","#801818","#CD7F32","#C0C0C0","gold","#B0E0E6"),guide=F)+
  theme_minimal()

RFM_persone_1_plot


################ MODELLO RFM DAL 2018/11/01 AL 2019/04/30 ########################

df_clienti_attivi2 <- df_clienti_attivi %>% 
  filter(TIC_DATE >= as.Date("2018-11-01")) %>%
  select(-LAST_DATE_PURCH, - DIFF_DAYS)

df_last_date_2_persone <- df_clienti_attivi2 %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_2_persone)
boxplot(df_last_date_2_persone$DIFF_DAYS)


df_clienti_attivi_2 <- merge(df_clienti_attivi2, df_last_date_2_persone, by="ID_CLI")

#notiamo cdal numero di righe dei due dataset he i clienti sono aumentati ma le transazioni ovvero il numero 
#di scontrini sono diminuite-

##Calcolo Recency: l'ultimo acquisto dopo quanto tempo?

quantili<- quantile(df_last_date_2_persone$DIFF_DAYS, probs = c(0.25, 0.50, 0.75))
quantili
#25% 50% 75% 
#24  58 114 

Recency_2_persone = df_last_date_2_persone %>% mutate(CLASS_R=case_when(DIFF_DAYS<quantili[1] ~ "Low",
                                                     (DIFF_DAYS>=quantili[1]) & (DIFF_DAYS<quantili[3])~"Medium",
                                                     (DIFF_DAYS>=quantili[3])~"High"))


Recency_2_persone = mutate(Recency_2_persone,CLASS_R=factor(CLASS_R,levels=c("Low","Medium","High")))


##Calcolo Frequency: ogni quanto acquisto?

Frequency_2_persone <- df_clienti_attivi_2 %>%
  filter(DIREZIONE==1) %>% group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>0)

boxplot(Frequency_2_persone$N_SCONTRINI_PER_CLI)

quantili<- quantile(Frequency_2_persone$N_SCONTRINI_PER_CLI,probs = c(0.50,0.70,0.90))
quantili
#50% 70% 90% 
#2   3   7 


Frequency_2_persone <- Frequency_2_persone %>% mutate(CLASS_F=case_when(N_SCONTRINI_PER_CLI<quantili[1] ~ "Low",
                                                           (N_SCONTRINI_PER_CLI>=quantili[1]) & (N_SCONTRINI_PER_CLI<quantili[3])~"Medium",
                                                           (N_SCONTRINI_PER_CLI>quantili[3])~"High"))
Frequency_2_persone = mutate(Frequency_2_persone,CLASS_F=factor(CLASS_F,levels=c("Low","Medium","High")))

#Monetary Value (differenza tra importo lordo e sconto)

Monetary_Value_2_persone = df_clienti_attivi_2 %>% filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(AMOUNT = sum(IMPORTO_LORDO) - sum(SCONTO))

quantili = quantile(Monetary_Value_2_persone$AMOUNT,probs = c(0.25,0.50,0.75))
quantili
#25%      50%      75% 
#48.4500 138.5500 388.9975 


Monetary_Value_2_persone = Monetary_Value_2_persone %>% mutate(CLASS_M = case_when(AMOUNT<quantili[1]~"Low",
                                                          (AMOUNT>=quantili[1])&(AMOUNT<quantili[3])~"Medium",
                                                          AMOUNT>quantili[3]~"High"))
Monetary_Value_2_persone = mutate(Monetary_Value_2_persone,CLASS_M=factor(CLASS_M,levels=c("Low","Medium","High")))

# Recency-Frequency

RF_persone_2 <- merge(Recency_2_persone, Frequency_2_persone,by="ID_CLI")
RF_persone_2 <- RF_persone_2 %>% mutate(CLASS_RF_persone_2 = case_when((CLASS_F=="Low")&(CLASS_R=="Low")~"One-Timer",
                                                       (CLASS_F=="Low")&(CLASS_R=="Medium")~"One-Timer",
                                                       (CLASS_F=="Low")&(CLASS_R=="High")~"Leaving",
                                                       (CLASS_F=="Medium")&(CLASS_R=="Low")~"Engaged",
                                                       (CLASS_F=="Medium")&(CLASS_R=="Medium")~"Engaged",
                                                       (CLASS_F=="Medium")&(CLASS_R=="High")~"Leaving",
                                                       (CLASS_F=="High")&(CLASS_R=="Low")~"Top",
                                                       (CLASS_F=="High")&(CLASS_R=="Medium")~"Top",
                                                       (CLASS_F=="High")&(CLASS_R=="High")~"Leaving Top"))

Fedelta_2_persone <- as.data.frame(with(RF_persone_2,table(CLASS_RF_persone_2)))
Fedelta_2_persone <- Fedelta_2_persone %>% mutate(CLASS_RF_persone_2 = factor(CLASS_RF_persone_2,levels = c("Leaving","One-Timer",
                                                                                      "Engaged","Leaving Top",
                                                                                      "Top")))

Fedelta_2_persone # la maggior parte sono Engaged, continuano a esserci molti One_Timer

Fedelta_2_persone$Freq <- (Fedelta_2_persone$Freq / nrow(RF_persone_2))

Fedelta_2_persone_plot <- 
  ggplot(Fedelta_2_persone,aes(CLASS_RF_persone_2,Freq,fill=CLASS_RF_persone_2)) + geom_bar(stat = "identity",width = 0.5)+
  ggtitle("Fedeltà")+ylab("Percentuale di Clienti")+ xlab("Fedeltà")+ scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Reds")+ 
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

Fedelta_2_persone_plot

################### rendere più carino

##### RFM

RFM_2_persone <- RF_persone_2 %>% select(-c("LAST_DATE_PURCH","CLASS_R","CLASS_F")) %>% 
  left_join(Monetary_Value_2_persone,by="ID_CLI")
RFM_2_persone <- mutate(RFM_2_persone,CLASS_RF_persone_2=factor(CLASS_RF_persone_2))


RFM_2_persone <- RFM_2_persone %>% mutate(CLASSI_2 = case_when((CLASS_M=="Low") & (CLASS_RF_persone_2=="One-Timer")~"Cheap",
                                          (CLASS_M=="Low") & (CLASS_RF_persone_2=="Leaving")~"Tin",
                                          (CLASS_M=="Low") & (CLASS_RF_persone_2=="Engaged")~"Copper",
                                          (CLASS_M=="Low") & (CLASS_RF_persone_2=="Leaving Top")~"Bronze",
                                          (CLASS_M=="Low") & (CLASS_RF_persone_2=="Top")~"Silver",
                                          (CLASS_M=="Medium") & (CLASS_RF_persone_2=="One-Timer")~"Tin",
                                          (CLASS_M=="Medium") & (CLASS_RF_persone_2=="Leaving")~"Copper",
                                          (CLASS_M=="Medium") & (CLASS_RF_persone_2=="Engaged")~"Bronze",
                                          (CLASS_M=="Medium") & (CLASS_RF_persone_2=="Leaving Top")~"Silver",
                                          (CLASS_M=="Medium") & (CLASS_RF_persone_2=="Top")~"Gold",
                                          (CLASS_M=="High") & (CLASS_RF_persone_2=="One-Timer")~"Copper",
                                          (CLASS_M=="High") & (CLASS_RF_persone_2=="Leaving")~"Bronze",
                                          (CLASS_M=="High") & (CLASS_RF_persone_2=="Engaged")~"Silver",
                                          (CLASS_M=="High") & (CLASS_RF_persone_2=="Leaving Top")~"Gold",
                                          (CLASS_M=="High") & (CLASS_RF_persone_2=="Top")~"Diamond"))

RFM_2_persone = RFM_2_persone %>% mutate(CLASSI_2 = factor(CLASSI_2,levels = c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")))
RFM_TOT2_persone <- as.data.frame(with(RFM_2_persone,table(CLASSI_2)))

RFM_TOT2_persone#per la maggior parte sono bronze e Tin e copper

RFM_TOT2_persone$Freq <- (RFM_TOT2_persone$Freq / nrow(RFM_2_persone))


#EXPLORATORY ANALYSIS of RFM's dataframe
RFM_2_persone_plot <- 
  ggplot(RFM_TOT2_persone,aes(CLASSI_2,Freq,fill=CLASSI_2)) + geom_bar(stat = "identity")+
  labs(title = "Customer's distribution",size=18)+ylab("Percentuale clienti")+ scale_y_continuous(labels = percent) +
  scale_fill_manual(values=c("black","#2F4F4F","#801818","#CD7F32","#C0C0C0","gold","#B0E0E6"),guide=F)+
  theme_minimal()

RFM_2_persone_plot

##################################################################################
##################################################################################


############

# MODELLO RFM per la categoria aziende

################

lastdate2 = df_7_aziende %>% summarise(max(TIC_DATE))
lastdate2=lastdate2[1,1]
## prima data di acquisto risale al 2018-05-01, l'ultima data di acquisto risale al 2019-04-30

#vogliamo trovare i clienti attivi (80%)

#troviamo l'ultimo giorno di acquisto per ogni cliente
#e quanti giorni sono passati dall'ultimo acquisto in generale

df_last_date_aziende <- df_7_aziende %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_aziende)
boxplot(df_last_date_aziende$DIFF_DAYS)

giorni_max=quantile(df_last_date_aziende$DIFF_DAYS, probs = c(0.80, 0.85, 0.90))
giorni_max

#decidiamo di considerare attivi l'ottanta percento dei clienti

giorni_max = giorni_max[1]

aziende_attive <- df_last_date_aziende %>% filter(DIFF_DAYS<=giorni_max)

df_aziende_attive <- merge( df_7_aziende, aziende_attive, by="ID_CLI")

################### MODELLO RFM DAL 2018/08/01 AL 2019/01/31 #######################

df_aziende_attive1 <- df_aziende_attive %>% 
  filter(TIC_DATE <= as.Date("2019-01-31") & TIC_DATE >= as.Date("2018-08-01")) %>%
  select(-LAST_DATE_PURCH, - DIFF_DAYS)

df_last_date_aziende_1 <- df_aziende_attive1 %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_aziende_1)
boxplot(df_last_date_aziende_1$DIFF_DAYS)


df_aziende_attive_1 <- merge(df_aziende_attive1, df_last_date_aziende_1, by="ID_CLI")

##Calcolo Recency: l'ultimo acquisto dopo quanto tempo?

quantili<- quantile(df_last_date_aziende_1$DIFF_DAYS, probs = c(0.25, 0.50, 0.75))
quantili
#25% 50% 75% 
#108 141 176 

Recency_aziende_1 = df_last_date_aziende_1 %>% mutate(CLASS_R=case_when(DIFF_DAYS<quantili[1] ~ "Low",
                                                        (DIFF_DAYS>=quantili[1]) & (DIFF_DAYS<quantili[3])~"Medium",
                                                        (DIFF_DAYS>=quantili[3])~"High"))


Recency_aziende_1 = mutate(Recency_aziende_1,CLASS_R=factor(CLASS_R,levels=c("Low","Medium","High")))


##Calcolo Frequency: ogni quanto acquisto?

Frequency_aziende_1 <- df_aziende_attive_1 %>%
  filter(DIREZIONE==1) %>% group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>0)

boxplot(Frequency_aziende_1$N_SCONTRINI_PER_CLI)

quantili<- quantile(Frequency_aziende_1$N_SCONTRINI_PER_CLI,probs = c(0.50,0.70,0.90))
quantili
#50% 70% 90% 
#2   4   9 

Frequency_aziende_1 <- Frequency_aziende_1 %>% mutate(CLASS_F=case_when(N_SCONTRINI_PER_CLI<quantili[1] ~ "Low",
                                                        (N_SCONTRINI_PER_CLI>=quantili[1]) & (N_SCONTRINI_PER_CLI<quantili[3])~"Medium",
                                                        (N_SCONTRINI_PER_CLI>quantili[3])~"High"))
Frequency_aziende_1 = mutate(Frequency_aziende_1,CLASS_F=factor(CLASS_F,levels=c("Low","Medium","High")))

#Monetary Value (differenza tra importo lordo e sconto)

Monetary_Value_aziende_1 = df_aziende_attive_1 %>% filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(AMOUNT = sum(IMPORTO_LORDO) - sum(SCONTO))

quantili = quantile(Monetary_Value_aziende_1$AMOUNT,probs = c(0.25,0.50,0.75))
quantili
#25%     50%     75% 
#89.130 251.985 689.870 

Monetary_Value_aziende_1 = Monetary_Value_aziende_1 %>% mutate(CLASS_M = case_when(AMOUNT<quantili[1]~"Low",
                                                                   (AMOUNT>=quantili[1])&(AMOUNT<quantili[3])~"Medium",
                                                                   AMOUNT>quantili[3]~"High"))
Monetary_Value_aziende_1 = mutate(Monetary_Value_aziende_1,CLASS_M=factor(CLASS_M,levels=c("Low","Medium","High")))

# Recency-Frequency

RF_aziende_1 <- merge(Recency_aziende_1, Frequency_aziende_1,by="ID_CLI")
RF_aziende_1 <- RF_aziende_1 %>% mutate(CLASS_RF_aziende_1 = case_when((CLASS_F=="Low")&(CLASS_R=="Low")~"One-Timer",
                                               (CLASS_F=="Low")&(CLASS_R=="Medium")~"One-Timer",
                                               (CLASS_F=="Low")&(CLASS_R=="High")~"Leaving",
                                               (CLASS_F=="Medium")&(CLASS_R=="Low")~"Engaged",
                                               (CLASS_F=="Medium")&(CLASS_R=="Medium")~"Engaged",
                                               (CLASS_F=="Medium")&(CLASS_R=="High")~"Leaving",
                                               (CLASS_F=="High")&(CLASS_R=="Low")~"Top",
                                               (CLASS_F=="High")&(CLASS_R=="Medium")~"Top",
                                               (CLASS_F=="High")&(CLASS_R=="High")~"Leaving Top"))

Fedelta_aziende_1 <- as.data.frame(with(RF_aziende_1,table(CLASS_RF_aziende_1)))
Fedelta_aziende_1 <- Fedelta_aziende_1 %>% mutate(CLASS_RF_aziende_1 = factor(CLASS_RF_aziende_1,levels = c("Leaving","One-Timer",
                                                                            "Engaged","Leaving Top",
                                                                            "Top")))
Fedelta_aziende_1
summary(Fedelta_aziende_1)

#per la maggior parte sono Engaged

Fedelta_aziende_1$Freq <- (Fedelta_aziende_1$Freq / nrow(RF_aziende_1))

Fedelta_aziende_1_plot <- 
  ggplot(Fedelta_aziende_1,aes(CLASS_RF_aziende_1,Freq,fill=CLASS_RF_aziende_1)) + geom_bar(stat = "identity",width = 0.5)+
  ggtitle("Fedeltà")+ylab("Percentuale di Clienti")+xlab("Fedeltà")+
  scale_fill_brewer(palette = "Reds")+ scale_y_continuous(labels = percent) +
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

Fedelta_aziende_1_plot

################### rendere più carino !!!!!!!!!!!!!!

##### RFM_aziende_1

RFM_aziende_1 <- RF_aziende_1 %>% select(-c("LAST_DATE_PURCH","CLASS_R","CLASS_F")) %>% 
  left_join(Monetary_Value_aziende_1,by="ID_CLI")
RFM_aziende_1 <- mutate(RFM_aziende_1,CLASS_RF_aziende_1=factor(CLASS_RF_aziende_1))


RFM_aziende_1 <- RFM_aziende_1 %>% mutate(CLASSI_aziende_1 = case_when((CLASS_M=="Low") & (CLASS_RF_aziende_1=="One-Timer")~"Cheap",
                                               (CLASS_M=="Low") & (CLASS_RF_aziende_1=="Leaving")~"Tin",
                                               (CLASS_M=="Low") & (CLASS_RF_aziende_1=="Engaged")~"Copper",
                                               (CLASS_M=="Low") & (CLASS_RF_aziende_1=="Leaving Top")~"Bronze",
                                               (CLASS_M=="Low") & (CLASS_RF_aziende_1=="Top")~"Silver",
                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_1=="One-Timer")~"Tin",
                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_1=="Leaving")~"Copper",
                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_1=="Engaged")~"Bronze",
                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_1=="Leaving Top")~"Silver",
                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_1=="Top")~"Gold",
                                               (CLASS_M=="High") & (CLASS_RF_aziende_1=="One-Timer")~"Copper",
                                               (CLASS_M=="High") & (CLASS_RF_aziende_1=="Leaving")~"Bronze",
                                               (CLASS_M=="High") & (CLASS_RF_aziende_1=="Engaged")~"Silver",
                                               (CLASS_M=="High") & (CLASS_RF_aziende_1=="Leaving Top")~"Gold",
                                               (CLASS_M=="High") & (CLASS_RF_aziende_1=="Top")~"Diamond"))
RFM_aziende_1 = RFM_aziende_1 %>% mutate(CLASSI_aziende_1 = factor(CLASSI_aziende_1,levels = c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")))
RFM_TOT_aziende_1 <- as.data.frame(with(RFM_aziende_1,table(CLASSI_aziende_1)))

RFM_TOT_aziende_1 #la maggior parte dei clienti appartengono alla categoria Bronze e TIN e Copper 

RFM_TOT_aziende_1$Freq <- (RFM_TOT_aziende_1$Freq / nrow(RFM_aziende_1))


#EXPLORATORY ANALYSIS of RFM's dataframe
RFM_aziende_1_plot <- 
  ggplot(RFM_TOT_aziende_1,aes(CLASSI_aziende_1,Freq,fill=CLASSI_aziende_1)) + geom_bar(stat = "identity")+
  labs(title = "Customer's distribution",size=18)+ylab("Percentuale clienti")+ scale_y_continuous(labels = percent)+
  scale_fill_manual(values=c("black","#2F4F4F","#801818","#CD7F32","#C0C0C0","gold","#B0E0E6"),guide=F)+
  theme_minimal()

RFM_aziende_1_plot


################ MODELLO RFM DAL 2018/11/01 AL 2019/04/30 ########################

df_aziende_attive2 <- df_aziende_attive %>% 
  filter(TIC_DATE >= as.Date("2018-11-01")) %>%
  select(-LAST_DATE_PURCH, - DIFF_DAYS)

df_last_date_2_aziende <- df_aziende_attive2 %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_2_aziende)
boxplot(df_last_date_2_aziende$DIFF_DAYS)


df_clienti_attivi_2 <- merge(df_aziende_attive2, df_last_date_2_aziende, by="ID_CLI")

#notiamo cdal numero di righe dei due dataset he i clienti sono aumentati ma le transazioni ovvero il numero 
#di scontrini sono diminuite-

##Calcolo Recency: l'ultimo acquisto dopo quanto tempo?

quantili<- quantile(df_last_date_2_aziende$DIFF_DAYS, probs = c(0.25, 0.50, 0.75))
quantili
#25% 50% 75% 
#24  57 116

Recency_2_aziende = df_last_date_2_aziende %>% mutate(CLASS_R=case_when(DIFF_DAYS<quantili[1] ~ "Low",
                                                        (DIFF_DAYS>=quantili[1]) & (DIFF_DAYS<quantili[3])~"Medium",
                                                        (DIFF_DAYS>=quantili[3])~"High"))


Recency_2_aziende = mutate(Recency_2_aziende,CLASS_R=factor(CLASS_R,levels=c("Low","Medium","High")))


##Calcolo Frequency: ogni quanto acquisto?

Frequency_2_aziende <- df_clienti_attivi_2 %>%
  filter(DIREZIONE==1) %>% group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>0)

boxplot(Frequency_2_aziende$N_SCONTRINI_PER_CLI)

quantili<- quantile(Frequency_2_aziende$N_SCONTRINI_PER_CLI,probs = c(0.50,0.70,0.90))
quantili
#50% 70% 90% 
#2   4   9 

Frequency_2_aziende <- Frequency_2_aziende %>% mutate(CLASS_F=case_when(N_SCONTRINI_PER_CLI<quantili[1] ~ "Low",
                                                        (N_SCONTRINI_PER_CLI>=quantili[1]) & (N_SCONTRINI_PER_CLI<quantili[3])~"Medium",
                                                        (N_SCONTRINI_PER_CLI>quantili[3])~"High"))
Frequency_2_aziende = mutate(Frequency_2_aziende,CLASS_F=factor(CLASS_F,levels=c("Low","Medium","High")))

#Monetary Value (differenza tra importo lordo e sconto)

Monetary_Value_2_aziende = df_clienti_attivi_2 %>% filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(AMOUNT = sum(IMPORTO_LORDO) - sum(SCONTO))

quantili = quantile(Monetary_Value_2_aziende$AMOUNT,probs = c(0.25,0.50,0.75))
quantili
#   25%    50%    75% 
#  75.0025 224.6700 620.6275 


Monetary_Value_2_aziende = Monetary_Value_2_aziende %>% mutate(CLASS_M = case_when(AMOUNT<quantili[1]~"Low",
                                                                   (AMOUNT>=quantili[1])&(AMOUNT<quantili[3])~"Medium",
                                                                   AMOUNT>quantili[3]~"High"))
Monetary_Value_2_aziende = mutate(Monetary_Value_2_aziende,CLASS_M=factor(CLASS_M,levels=c("Low","Medium","High")))

# Recency-Frequency

RF_aziende_2 <- merge(Recency_2_aziende, Frequency_2_aziende,by="ID_CLI")
RF_aziende_2 <- RF_aziende_2 %>% mutate(CLASS_RF_aziende_2 = case_when((CLASS_F=="Low")&(CLASS_R=="Low")~"One-Timer",
                                               (CLASS_F=="Low")&(CLASS_R=="Medium")~"One-Timer",
                                               (CLASS_F=="Low")&(CLASS_R=="High")~"Leaving",
                                               (CLASS_F=="Medium")&(CLASS_R=="Low")~"Engaged",
                                               (CLASS_F=="Medium")&(CLASS_R=="Medium")~"Engaged",
                                               (CLASS_F=="Medium")&(CLASS_R=="High")~"Leaving",
                                               (CLASS_F=="High")&(CLASS_R=="Low")~"Top",
                                               (CLASS_F=="High")&(CLASS_R=="Medium")~"Top",
                                               (CLASS_F=="High")&(CLASS_R=="High")~"Leaving Top"))

Fedelta_2_aziende <- as.data.frame(with(RF_aziende_2,table(CLASS_RF_aziende_2)))
Fedelta_2_aziende <- Fedelta_2_aziende %>% mutate(CLASS_RF_aziende_2 = factor(CLASS_RF_aziende_2,levels = c("Leaving","One-Timer",
                                                                            "Engaged","Leaving Top",
                                                                            "Top")))

Fedelta_2_aziende # la maggior parte sono Engaged e leaving, continuano a esserci molti One_Timer

Fedelta_2_aziende$Freq <- (Fedelta_2_aziende$Freq / nrow(RF_aziende_2))

Fedelta_2_aziende_plot <- 
  ggplot(Fedelta_2_aziende,aes(CLASS_RF_aziende_2,Freq,fill=CLASS_RF_aziende_2)) + geom_bar(stat = "identity",width = 0.5)+
  ggtitle("Fedeltà")+ylab("Percentuale di Clienti")+ xlab("Fedeltà")+ scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Reds")+ 
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

Fedelta_2_aziende_plot

################### rendere più carino e mettere in percentuale

##### RFM

RFM_2_aziende <- RF_aziende_2 %>% select(-c("LAST_DATE_PURCH","CLASS_R","CLASS_F")) %>% 
  left_join(Monetary_Value_2_aziende,by="ID_CLI")
RFM_2_aziende <- mutate(RFM_2_aziende,CLASS_RF_aziende_2=factor(CLASS_RF_aziende_2))


RFM_2_aziende <- RFM_2_aziende %>% mutate(CLASSI_2 = case_when((CLASS_M=="Low") & (CLASS_RF_aziende_2=="One-Timer")~"Cheap",
                                               (CLASS_M=="Low") & (CLASS_RF_aziende_2=="Leaving")~"Tin",
                                               (CLASS_M=="Low") & (CLASS_RF_aziende_2=="Engaged")~"Copper",
                                               (CLASS_M=="Low") & (CLASS_RF_aziende_2=="Leaving Top")~"Bronze",
                                               (CLASS_M=="Low") & (CLASS_RF_aziende_2=="Top")~"Silver",
                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_2=="One-Timer")~"Tin",
                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_2=="Leaving")~"Copper",
                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_2=="Engaged")~"Bronze",
                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_2=="Leaving Top")~"Silver",
                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_2=="Top")~"Gold",
                                               (CLASS_M=="High") & (CLASS_RF_aziende_2=="One-Timer")~"Copper",
                                               (CLASS_M=="High") & (CLASS_RF_aziende_2=="Leaving")~"Bronze",
                                               (CLASS_M=="High") & (CLASS_RF_aziende_2=="Engaged")~"Silver",
                                               (CLASS_M=="High") & (CLASS_RF_aziende_2=="Leaving Top")~"Gold",
                                               (CLASS_M=="High") & (CLASS_RF_aziende_2=="Top")~"Diamond"))

RFM_2_aziende = RFM_2_aziende %>% mutate(CLASSI_2 = factor(CLASSI_2,levels = c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")))
RFM_TOT2_aziende <- as.data.frame(with(RFM_2_aziende,table(CLASSI_2)))

RFM_TOT2_aziende#per la maggior parte sono bronze e Tin

RFM_TOT2_aziende$Freq <- (RFM_TOT2_aziende$Freq / nrow(RFM_2_aziende))


#EXPLORATORY ANALYSIS of RFM's dataframe
RFM_2_aziende_plot <- 
  ggplot(RFM_TOT2_aziende,aes(CLASSI_2,Freq,fill=CLASSI_2)) + geom_bar(stat = "identity")+
  labs(title = "Customer's distribution",size=18)+ylab("Percentuale clienti")+ scale_y_continuous(labels = percent) +
  scale_fill_manual(values=c("black","#2F4F4F","#801818","#CD7F32","#C0C0C0","gold","#B0E0E6"),guide=F)+
  theme_minimal()

RFM_2_aziende_plot

##################################################################################
##################################################################################





####  CHURN PREDICTION MODEL

# Decidiamo la data di riferimento nel passato

# Il primo scontrino è stato emesso il 2018-05-01 e l'ultimo scontrino è stato emesso il 2019-04-30

df_7_persone$TIC_DATE <- as.Date(df_7_persone$TIC_DATE)
max(df_7_persone$TIC_DATE)
min(df_7_persone$TIC_DATE)

df <- df_7_persone

df1 <- df %>% 
  filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>1) #quanti acquisti per ogni cliente

df2 <- df %>%
  filter(DIREZIONE==1) %>%
  group_by(ID_SCONTRINO) %>% 
  summarise(ID_CLI = max(ID_CLI),TIC_DATE=max(TIC_DATE))

df3 <- left_join(df1,df2,by="ID_CLI") #aggiungiamo id_scontrino e la data

df4 <- df3 %>% 
  arrange(desc(TIC_DATE)) %>% 
  group_by(ID_CLI) %>% 
  summarise(last=nth(TIC_DATE,1),secondl=nth(TIC_DATE,2)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(last,secondl,units = "days")))


q <- ggplot(df4, aes(as.numeric(last-secondl), cumsum(stat(count)/nrow(df4)))) +
  geom_freqpoly(binwidth = 8,alpha=0.8,col="black") +
  labs(title = "Percentuale cumulativa di riacquisto", x = "days", y = "Cumulative Percentage of Repurchase") +
  geom_line(data = data.frame(days=1:365,const=0.80),aes(days,const),col="blue") +
  geom_line(data = data.frame(y=seq(0,1,0.1),x=72),aes(x,y),col="blue") +
  scale_x_continuous(breaks=seq(0,300,30)) +
  theme_classic()

q

# l'80% dei clienti riacquista entro 72 giorni

plot <- ggplot(df4, aes(x= DIFF_DAYS)) + 
  geom_histogram(color="#003399", fill="#99CBFF") +
  geom_vline(aes(xintercept = 72), color="#800000", linetype="dashed", size=1) +
  labs(title = "Ultimo acquisto - penultimo acquisto", x = "Intervallo di tempo", y = "Frequenza") +
  scale_x_continuous(breaks=seq(0,300,30)) +
  theme_minimal()

plot

# consideriamo un cliente come churn i clienti che non riacquistano entro tre mesi (decidiamo di prendere un campione leggermente più largo) ovvero nel periodo di holdout 
reference_date <- ymd(20190201)

#creiamo la colonna dei churner
df_churn <- df_7_persone %>%
  filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarize(LAST_PURCHASE_DATE = max(TIC_DATE),
            TOTAL_PURCHASE = sum(IMPORTO_LORDO),
            NUMBER_OF_PURCHASE=n())   %>%
  mutate(CHURN = as.numeric(LAST_PURCHASE_DATE >= as.Date("2019-02-01"))) %>%
  select(CHURN,ID_CLI,LAST_PURCHASE_DATE,TOTAL_PURCHASE,NUMBER_OF_PURCHASE)

sum(df_churn$CHURN==1)
sum(df_churn$CHURN==0)

#85829 clienti non sono churn, 103997 sono churn

# the length of a holdout period after each reference date.

holdout_period <- df_7_persone %>% 
  filter(TIC_DATE >= as.Date("2019-02-01")) %>%
  filter(DIREZIONE==1)                                               

#The holdout period starts on the 2019-02-01 and it ends at the 2019-04-30.

#Third STEP: Choosing the lenght of a lookback period before the reference date:
#consideriamo come lunghezza del periodo di lookback di cinque mesi

lookback_period  <- df_7_persone %>% 
  filter(TIC_DATE < as.Date("2019-02-01") &TIC_DATE >= as.Date("2018-08-01") & DIREZIONE==1)


### scegliamo le variabili da inserire

#RECENCY:
Recency_churn <- lookback_period%>% 
  group_by(ID_CLI)%>%
  summarise(Last_date=max(TIC_DATE),
  Recency=as.numeric(difftime(reference_date,Last_date),units="days"))

#FREQUENCY:
Frequency_churn <- lookback_period %>% 
  group_by(ID_CLI) %>% summarise(Frequency=n_distinct(ID_SCONTRINO))

#MONETARY:
Monetary_churn <- lookback_period %>% 
  group_by(ID_CLI)%>%
  summarise(Imp_lordo = sum(IMPORTO_LORDO), 
            Sconto = sum(SCONTO), 
            SPESA_NETTA = Imp_lordo-Sconto) %>%
  select(ID_CLI, SPESA_NETTA)


#These variables concern the customer behaviour.
Churn <- merge(Recency_churn,Frequency_churn,by="ID_CLI")

Churn <- merge(Churn,Monetary_churn, by="ID_CLI")

df_churn2 <- df_7_persone %>%
  filter(TIC_DATE < as.Date("2019-02-01") &TIC_DATE >= as.Date("2018-08-01") & DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarize(NUMBER_OF_PURCHASE=n()) %>% 
  select(ID_CLI,NUMBER_OF_PURCHASE)


Churn2 <- df_churn %>% select(ID_CLI, CHURN)

Churn2 <- merge(df_churn2, Churn2, by= "ID_CLI")

Churn3 <- merge(Churn, Churn2, by= "ID_CLI")

Churn_RFM <- RFM_persone_1 %>% select(ID_CLI, CLASS_RF_persone_1, CLASSI_persone_1)

Churn_4 <- Churn_RFM %>%  right_join (Churn3, by="ID_CLI")

Churn_4$CHURN <- as.factor(Churn_4$CHURN)

df_totale <- df_1_persone %>% 
  select(ID_CLI, LAST_COD_FID, LAST_STATUS_FID, LAST_DT_ACTIVE) %>%
  left_join(df_2_persone %>%
              select(ID_CLI, W_PHONE, TYP_JOB), by= "ID_CLI") %>%
  left_join(df4_persone, by = "ID_CLI")

df_finale<- df_totale %>% right_join(Churn_4, by= "ID_CLI")

df_6_p <- df_6_persone %>% filter(OPENED == TRUE) %>% group_by(ID_CLI) %>%
  summarise(N_EMAIL_APERTE= n())
  
df_6_p2 <- df_6_persone %>% filter(CLICKED == TRUE) %>% group_by(ID_CLI) %>%
  summarise(N_EMAIL_CLICCATE= n())

df_finale <- df_finale %>% left_join(df_6_p, by= "ID_CLI") %>% left_join(df_6_p2, by="ID_CLI")

df_finale2 <- df_finale %>% mutate(N_EMAIL_APERTE = fct_explicit_na(N_EMAIL_APERTE, "0"))%>% mutate(N_EMAIL_CLICCATE = fct_explicit_na(N_EMAIL_CLICCATE, "0"))

# se i clienti hanno mandato qualcosa 
#per essere rimborsati??






library(e1071)
library(caret)
library(pander)

summary(df_finale)

# 78364 clienti churner, 47620 clienti non churner

#Train e Test set

train_index <- createDataPartition(df_finale$CHURN,p=0.70,list = FALSE, times=1)
train <- df_finale[train_index,]
test <- df_finale[-train_index,]

table(train$CHURN)

#il train è sbilanciato

ggplot(data=df_finale, aes(x=CHURN)) +
  geom_bar( fill="lightblue", color="black") +
  labs(x="No churn (0) / churn (1)", y="Number of Customers",
       title="Sbilanciamento Classi") +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

#The churn phenomenon is a classification problem with class imbalance.
#We need to solve classes imbalance problem

churn0 <- train %>% filter(CHURN == 0) #contiene 54855 righe
churn1 <- train %>% filter(CHURN == 1) #contiene 33334 righe

balance <- churn1[sample(nrow(churn1), nrow(churn0), replace = TRUE),] 
train_balanced <- rbind(balance, churn0)

table(train_balanced$CHURN) 

#ora abbiamo una classe bilanciata

#Seventh STEP: Implementing classification algorithms.
#There are several machine learning algorithms that can be used for supervised propensity models.
#For a given set of input predictors corresponding to a customer and specific reference date, the output generated by any of these algorithms is a numeric value bounded between [0,1]

library(rpart)
library(rpart.plot)
library(MLmetrics)
library(randomForest)
library(Rcmdr)
library(glmnet)




#1. RANDOM FOREST
memory.limit(10000)
rf <- randomForest(CHURN~.,data=train,ntree=100)
#Prediction Random Forest
pred_rf<-predict(rf,test[,-5],type = "class")
prob_rf <- predict(rf,test[,-5],type = "prob")[,1]
cm_rf<-confusionMatrix(pred_rf,test$CHURN)
rec_rf <- round(recall(pred_rf,test$CHURN,relevant=1),3)
prec_rf <- round(precision(pred_rf,test$CHURN,relevant=1),3)
f1_rf <- round(F1_Score(pred_rf,test$CHURN),3)
acc_rf <- round(Accuracy(pred_rf,test$CHURN),3)
#___________________________________________________________________________________________________________________________________________________



#2. DECISION TREES
tree<-rpart(CHURN~.,data = train)
rpart.plot(tree) #, extra = "auto")
summary(tree) #num di acquisti è la variabile più importante
printcp(tree)


#prediction Decision Trees
pred_dt <- predict(dec_tree,test[,-5],type = "class")
prob_dt <-predict(dec_tree,test[,-5],type = "prob")[,1]
cm_dt<-confusionMatrix(pred_dt,test$CHURN)
rec_dt <- round(recall(pred_dt,test$CHURN,relevant=1),3)
prec_dt <- round(precision(pred_dt,test$CHURN,relevant=1),3)
f1_dt <- round(F1_Score(pred_dt,test$CHURN),3)
acc_dt <- round(Accuracy(pred_dt,test$CHURN),3)



#___________________________________________________________________________________________________________________________________________________
#3. NAIVE BAYES
naive <- naiveBayes(CHURN~.,data = train)
#prediction Naive Bayes
pred_naive <- predict(naive,test[,-5])
prob_naive <- predict(naive,test[,-5], type = "raw")[,1]
cm_nb<-confusionMatrix(test$CHURN,pred_naive)
rec_nb <- round(recall(pred_naive,test$CHURN,relevant=1),3)
prec_nb <- round(precision(pred_naive,test$CHURN,relevant=1),3)
f1_nb <- round(F1_Score(pred_naive,test$CHURN),3)
acc_nb <- round(Accuracy(pred_naive,test$CHURN),3)
#___________________________________________________________________________________________________________________________________________________
#4.LOGISTIC REGRESSION
log<-train(CHURN~.,data = train,method = "glm")
summary(log)
#prediction Logistic Regression
pred_log<- predict(log,test[,-5],type="raw")
prob_log<-predict(log,test[,-5],type="prob")[,1]
cm_log<-confusionMatrix(pred_log,test$CHURN)
rec_log <- round(recall(pred_log,test$CHURN,relevant=1),3)
prec_log <- round(precision(pred_log,test$CHURN,relevant=1),3)
f1_log <- round(F1_Score(pred_log,test$CHURN),3)
acc_log <- round(Accuracy(pred_log,test$CHURN),3)
#___________________________________________________________________________________________________________________________________________________

#5.BAGGING

bag <- bagging(CHURN~.,data = train, nbagg=25)

#prediction Bagging

pred_bag<-as.factor(predict(bag, test[,-5])$class)

prob_bag <- predict(bag, test[,-5])$prob[,1]

cm_bag<-confusionMatrix(pred_bag, test$CHURN)

rec_bag <- round(recall(pred_bag, test$CHURN,relevant=1),3)

prec_bag <- round( precision(pred_bag,test$CHURN, relevant=1) ,3)

f1_bag <- round(F1_Score(pred_bag, test$CHURN),3)

acc_bag <- round(Accuracy(pred_bag, test$CHURN),3)


#___________________________________________________________________________________________________________________________________________________
#The best modelling approach is identified by benchmarking the perfomance metrics precisions, recall, recall, F1-score, AUC and lift.
measure_matrix=matrix(0,ncol = 4,nrow = 5)
colnames(measure_matrix) <- c("Recall","Precision","F1_Score","Accuracy")
rownames(measure_matrix) <- c("Random forest","Decision trees",
                              "Logistic regression","Naive bayes","Bagging")
measure_matrix[1,] <- c(rec_rf,prec_rf,f1_rf,acc_rf)
measure_matrix[2,] <- c(rec_dt,prec_dt,f1_dt,acc_dt)
measure_matrix[3,] <- c(rec_log,prec_log,f1_log,acc_log)
measure_matrix[4,] <- c(rec_nb,prec_nb,f1_nb,acc_nb)
measure_matrix[5,] <- c(rec_bag,prec_bag,f1_bag,acc_bag)
measure_df <- as.data.frame(measure_matrix)

#ACCURACY PLOT:
accuracy_df <- as.data.frame(cbind(rownames(measure_df),measure_df$Accuracy))
colnames(accuracy_df) <- c("Method","Value")

plot_accuracy<-ggplot(accuracy_df,aes(x=Method,y=Value,fill=Method)) +
  geom_bar(stat = "identity") + scale_fill_brewer(palette = "BuGn")+
  ggtitle("ACCURACY PLOT")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

plot_accuracy

#F1-score PLOT:
f1score_df <- as.data.frame(cbind(rownames(measure_df),measure_df$F1_Score))
colnames(f1score_df) <- c("Method","Value")

plot_f1<-ggplot(f1score_df,aes(x=Method,y=Value,fill=Method)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "BuGn")+
  ggtitle("F1 SCORE PLOT")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))

plot_f1

#ROC CURVES:
roc_rf <- roc(test$CHURN ~ as.numeric(unlist(pred_rf)),plot=T,
              print.auc=TRUE,col="blue",lwd =4,legacy.axes=TRUE)
roc_nb <- roc(test$CHURN ~ as.numeric(unlist(pred_naive)),plot=TRUE,
              print.auc=TRUE,col="green",lwd = 4,print.auc.y=0.1,
              legacy.axes=TRUE,add = TRUE)
roc_log <- roc(test$CHURN ~ as.numeric(unlist(pred_log)),plot=TRUE,
               print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.3,
               legacy.axes=TRUE,add = TRUE)
roc_dt <- roc(test$CHURN ~ as.numeric(unlist(pred_dt)),plot=TRUE,
              print.auc=TRUE,col="cyan",lwd = 4,print.auc.y=0.4,
              legacy.axes=TRUE,add = TRUE)
roc_bag <- roc(test$CHURN ~ as.numeric(unlist(pred_bag)),plot=TRUE,
               print.auc=TRUE,col="orange",lwd = 4,print.auc.y=0.2,
               legacy.axes=TRUE,add = TRUE)

legend("right",legend=c("RF", "LOG", "DT", "NB","BAG"),fill =c("blue","red", "cyan", "green", "orange"), 
       cex = .75, inset = .1, bty = "n")

#LIFT Measure: 

lift_class <- as.data.frame(cbind(prob_bag, prob_dt, prob_naive, prob_rf, prob_log))
lift_class <- cbind(lift_class, test$CHURN)
colnames(lift_class)[6]="churn"
lift_bag <- gain_lift(data = lift_class, score ="prob_bag" , target = "churn" )
lift_dt <- gain_lift(data = lift_class, score ="prob_dt" , target = "churn" )
lift_naive <- gain_lift(data = lift_class, score ="prob_naive" , target = "churn" )
lift_rf <- gain_lift(data = lift_class, score ="prob_rf" , target = "churn" )
lift_log <- gain_lift(data = lift_class, score ="prob_log" , target = "churn" )














