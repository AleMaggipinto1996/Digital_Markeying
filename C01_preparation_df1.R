#### PREPARAZIONE DEI DATASET ####

### Dataset n?1 ###

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

### Numero di programmi fedelt? per numero di clienti ##
##quante sottoscrizioni ho per ciascun cliente?

num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID)
            , NUM_DATEs = n_distinct(DT_ACTIVE)
  )

#Controllando le date, emerge che vi sono clienti hanno probabilmente sbagliato/cambiato la fidelizzazione pi? volte
#nello stesso giorno

tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)

## compute the distribution of number of subscriptions
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

## Ci sono clienti con molteplici programmi fedelt? ##
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

#ID 1 ? l'ID relativo alla registrazione online, gli altri sono negozi


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

#creo una nuova colonna 0/1: ? 1 se si tratta di registrazione online, 
#? 0 se ? stata fatta nel negozio

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

#? importante capire il modo migliore per presentare i dati

##### SEPARIAMO IN DUE DATASET: AZIENDE/PERSONE ####

df_1_aziende <- df_1_cli_fid_clean %>%  
  filter(LAST_COD_FID == 'PREMIUM BIZ' | LAST_COD_FID == 'STANDARD BIZ')

df_1_persone <- df_1_cli_fid_clean %>%  
  filter(LAST_COD_FID == 'PREMIUM' | LAST_COD_FID == 'STANDARD')

#_________________________________________

############# consideriamo la parte che riguarda le persone fisiche ###########?

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
  group_by(NegOnline) %>%
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


##Distribuzione dei clienti sulla base dello STATUS della tessera fedelta'
##(1 se e' attivo, 0 se o inattivo)
dist_df1_status <- df_1_cli_fid_clean %>% group_by(LAST_STATUS_FID) %>%
  summarise(TOT_CLIENTI = n_distinct(ID_CLI)) %>%
  mutate( PERC_CLIENTI = TOT_CLIENTI/sum(TOT_CLIENTI))

dist_df1_status
#Il 99.1% delle tessere fedelta' sono attive, solo lo 0.9% non
#lo sono

plot_dist_df1_status <- ggplot(dist_df1_status,aes(x=LAST_STATUS_FID,y=TOT_CLIENTI)) +
  xlab("STATO DI ATTIVAZIONE") + theme_minimal() + 
  geom_bar(stat = "identity",aes(fill=LAST_STATUS_FID)) +
  scale_fill_manual(values = c("red","darkgreen"),guide=F)

plot_dist_df1_status

##Distribuzione dei clienti sulla base della TIPO DI TESSERA FEDELT?
##0 se ? un account secondario, 1 se ? l'account principale

dist_df1_typfid <- df_1_cli_fid_clean %>% group_by(LAST_TYP_CLI_FID) %>%
  summarise(TOT_CLIENTI = n_distinct(ID_CLI)) %>%
  mutate(PERC_CLIENTI = TOT_CLIENTI/sum(TOT_CLIENTI))

dist_df1_typfid
#L'ultima tessera fedelt? attivata dai clienti nel 98.5% dei casi
#? una tessera principale (363862 casi totali), mentre nell'1.5% dei casi
#(5610 casi totali) abbiamo che la tessera attivata ? secondaria

plot_dist_df1_typfid <- ggplot(dist_df1_typfid,aes(x=LAST_TYP_CLI_FID,y=TOT_CLIENTI)) +
  xlab("TESSERE NON PRINCIPALI/PRINCIPALI") + theme_minimal() + 
  geom_bar(stat = "identity",aes(fill=LAST_TYP_CLI_FID)) +
  scale_fill_manual(values = c("grey","gold"),guide=F)

plot_dist_df1_typfid

### Variabile LAST_DT_ACTIVE###
##Consideriamo la variabile LAST_DT_ACTIVE, corrispondente al giorno in cui
##un cliente ha attivato la sua ultima tessera fedelt?.
##Le attivazioni sono state raggruppate in base ai mesi.

dist_df1_active_by_month <- df_1_cli_fid_clean %>%
  mutate(MONTH= month(LAST_DT_ACTIVE),
         YEAR = year(LAST_DT_ACTIVE)) %>%
  group_by(YEAR,MONTH) %>%
  summarise(TOT_SUBs = n_distinct(ID_CLI)) %>%
  mutate(YEAR_MONTH = as.factor(paste0(YEAR,"/",MONTH))) %>%
  arrange(YEAR,MONTH) %>%
  mutate(YEAR_MONTH = factor(YEAR_MONTH, levels = c("2018/1","2018/2",
                                                    "2018/3","2018/4",
                                                    "2018/5","2018/6",
                                                    "2018/7","2018/8",
                                                    "2018/9","2018/10",
                                                    "2018/11","2018/12",
                                                    "2019/1","2019/2",
                                                    "2019/3","2019/4",
                                                    "2019/5")))


dist_df1_active_by_month

plot_dist_df1_active_by_month<- 
  ggplot(dist_df1_active_by_month,aes(x=YEAR_MONTH,TOT_SUBs)) +
  xlab("MESE DI ATTIVAZIONE") + theme_minimal() + 
  geom_bar(stat = "identity",fill="darkgreen")

plot_dist_df1_active_by_month

#I mesi in cui ci sono maggiori sottoscrizioni sono marzo 2018 e novembre 2018
#da novembre 2018 in poi c'? un crollo sostanziale delle sottoscrizioni.
#Anche nel 2019 marzo ? il mese che presenta maggiori sottoscrizioni

##Distribuzione numero di clienti per negozio
df1_dist_neg <- df_1_cli_fid_clean %>%
  group_by(FIRST_ID_NEG) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs),
         FIRST_ID_NEG = as.factor(FIRST_ID_NEG))

df1_dist_neg

plot_df1_dist_neg <- ggplot(df1_dist_neg,aes(FIRST_ID_NEG,TOT_CLIs))+
  geom_bar(stat = "identity",fill = "orange")

plot_df1_dist_neg

#Il negozio 1 ? quello che presenta un maggior numero di clienti
#da solo comprende il 15.8% totale della clientela.

#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)


