
df_1_aziende <- df_1_cli_fid_clean %>%  
  filter(LAST_COD_FID == 'PREMIUM BIZ' | LAST_COD_FID == 'STANDARD BIZ')

#_________________________________________

############# consideriamo la parte che riguarda i clienti business ###########

#_________________________________________


### variable LAST_DT_ACTIVE per MESI ###

## compute distribution 
df1_aziende_codfid_ld <- df_1_aziende %>%
  group_by(substring(LAST_DT_ACTIVE,1,7)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Mesi = substring(LAST_DT_ACTIVE,1,7))

df1_aziende_codfid_ld


## plot distribution

plot_df1_aziende_codfid_ld <- (
  ggplot(data=df1_aziende_codfid_ld
         , aes(x=Mesi, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_aziende_codfid_ld


### variable LAST_DT_ACTIVE per ANNI ###

## compute distribution 
df1_az_codfid_ld <- df_1_aziende %>%
  group_by(substring(LAST_DT_ACTIVE,1,4)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Year = `substring(LAST_DT_ACTIVE, 1, 4)`)

df1_az_codfid_ld


## plot distribution

plot_df1_az_codfid_ld <- (
  ggplot(data=df1_az_codfid_ld
         , aes(x=Year, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_az_codfid_ld


### variable FIRST_DT_ACTIVE per MESI ###

## compute distribution
df1_az_codfid_fd <- df_1_aziende %>%
  group_by(substring(FIRST_DT_ACTIVE,1,7)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Mesi = `substring(FIRST_DT_ACTIVE, 1, 7)`)

df1_az_codfid_fd

## plot distribution

plot_df1_az_codfid_fd <- (
  ggplot(data=df1_az_codfid_fd
         , aes(x=Mesi, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_az_codfid_fd



### variable FIRST_DT_ACTIVE per ANNI ###

## compute distribution
df1_az_codfid_fd <- df_1_aziende %>%
  group_by(substring(FIRST_DT_ACTIVE,1,4)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Year = `substring(FIRST_DT_ACTIVE, 1, 4)`)

df1_az_codfid_fd

## plot distribution

plot_df1_az_codfid_fd <- (
  ggplot(data=df1_az_codfid_fd
         , aes(x=Year, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_az_codfid_fd


### variable LAST_STATUS_FID ###

## compute distribution
df1_az_codfid_status <- df_1_aziende %>%
  group_by(LAST_STATUS_FID) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))


df1_az_codfid_status

## plot distribution

plot_df1_az_codfid_status <- (
  ggplot(data=df1_az_codfid_status
         , aes(x=LAST_STATUS_FID, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_az_codfid_status


### variable NUM_FIDS ###

## compute distribution
df1_az_codfid_n <- df_1_aziende%>%
  group_by(NUM_FIDs) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_az_codfid_n

## plot distribution

plot_df1_az_codfid_n <- (
  ggplot(data=df1_az_codfid_n
         , aes(x=NUM_FIDs, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_az_codfid_n

### variable LAST_TYP_CLI_FID ###

## compute distribution
df1_az_codfid_main <- df_1_aziende %>%
  group_by(LAST_TYP_CLI_FID) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_az_codfid_main

## plot distribution

plot_df1_az_codfid_main <- (
  ggplot(data=df1_az_codfid_main
         , aes(x=LAST_TYP_CLI_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_az_codfid_main


### variabile Negozio Online / Negozio Fisico ###

## compute distribution
df1_az_codfid_neg <- df_1_aziende %>%
  group_by(RegOnline) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_az_codfid_neg

## plot distribution

plot_df1_az_codfid_neg <- (
  ggplot(data=df1_az_codfid_neg
         , aes(x=RegOnline, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_az_codfid_neg


#### FINAL REVIEW df_1_clean ####

str(df_1_aziende)
summary(df_1_aziende)

#________________________________________________

######### SONO LE STESSE COSE CHE ABBIAMO FATTO NOI ###########

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

##Distribuzione dei clienti sulla base della TIPO DI TESSERA FEDELTA'
##0 se e' un account secondario, 1 se e' l'account principale

dist_df1_typfid <- df_1_cli_fid_clean %>% group_by(LAST_TYP_CLI_FID) %>%
  summarise(TOT_CLIENTI = n_distinct(ID_CLI)) %>%
  mutate(PERC_CLIENTI = TOT_CLIENTI/sum(TOT_CLIENTI))

dist_df1_typfid
#L'ultima tessera fedelt? attivata dai clienti nel 98.5% dei casi
#e' una tessera principale (363862 casi totali), mentre nell'1.5% dei casi
#(5610 casi totali) abbiamo che la tessera attivata e' secondaria

plot_dist_df1_typfid <- ggplot(dist_df1_typfid,aes(x=LAST_TYP_CLI_FID,y=TOT_CLIENTI)) +
  xlab("TESSERE NON PRINCIPALI/PRINCIPALI") + theme_minimal() + 
  geom_bar(stat = "identity",aes(fill=LAST_TYP_CLI_FID)) +
  scale_fill_manual(values = c("grey","gold"),guide=F)

plot_dist_df1_typfid

### Variabile LAST_DT_ACTIVE###
##Consideriamo la variabile LAST_DT_ACTIVE, corrispondente al giorno in cui
##un cliente ha attivato la sua ultima tessera fedelta'.
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
