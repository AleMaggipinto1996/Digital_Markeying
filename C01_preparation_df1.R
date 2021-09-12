#### PREPARAZIONE DEI DATASET ####

### Dataset n1 ###

###primo sguardo al dataset

str(df_1_cli_fid)  #come sono fatti i nostri dati
summary(df_1_cli_fid)

#si vede che la maggior parte dei clienti hanno un account ed Ã¨ attivo

## START CLEANING df_1 ##

### Ricreare il dataset

df_1_cli_fid_clean <- df_1_cli_fid

### Check for duplicates (non duplicati per CLI-FID)
df_1_cli_fid_clean %>% summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
                                 , TOT_ID_FIDs = n_distinct(ID_FID)
                                 , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
                                 , TOT_ROWs = n())

#ci sono piu' registrazioni di carte fedelta' di ciascun cliente e questo non ci sorprende

### Formattazione delle date 
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

### Formattazione boleani in fattori ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

### Numero di programmi fedelta' per numero di clienti ##
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

#ID 1 è quello relativo alla registrazione online, gli altri sono negozi


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
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## plot distribution
plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_COD_FID, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue",colour="black") +
     theme_minimal())

ggplotly(plot_df1_dist_codfid)



plot_d <- ggplot(data=df1_dist_codfid
         , aes(x=LAST_COD_FID, y=PERCENT,fill=LAST_COD_FID)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("PREMIUM" = "blue4",
                                 "PREMIUM BIZ" = "green4",
                                 "STANDARD" = "blue",
                                 "STANDARD BIZ" = "green"))
                  

ggplotly(plot_d)


#si vede una sproporzione elevata tra quelli che hanno una fidelizzazione standard e 
#quelli premium

#Il suffisso BIZ sta ad indicare i clienti business e quindi consente di distinguere
#tra AZIENDE e PERSONE FISICHE


##### SEPARIAMO IN DUE DATASET: AZIENDE/PERSONE ####

df_1_aziende <- df_1_cli_fid_clean %>%  
  filter(LAST_COD_FID == 'PREMIUM BIZ' | LAST_COD_FID == 'STANDARD BIZ')

df_1_persone <- df_1_cli_fid_clean %>%  
  filter(LAST_COD_FID == 'PREMIUM' | LAST_COD_FID == 'STANDARD')

#_________________________________________

############# consideriamo la parte che riguarda le persone fisiche ###########

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

ggplotly(plot_df1_persone_codfid_ld)

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

ggplotly(plot_df1_p_codfid_ld)



### variable LAST_STATUS_FID ###

## compute distribution
df1_p_codfid_status <- df_1_persone %>%
  group_by(LAST_STATUS_FID) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
  arrange(desc(PERCENT))


df1_p_codfid_status  # il 99.2% dei clienti "persone" ha una tessera fedeltà attiva

## plot distribution

plot_df1_p_codfid_status <- (
  ggplot(data=df1_p_codfid_status
         , aes(x=LAST_STATUS_FID, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

ggplotly(plot_df1_p_codfid_status)


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

ggplotly(plot_df1_p_codfid_n)

## variabile LAST_TYP_CLI_FID
##0 se è un account secondario, 1 se è l'account principale

## compute distribution
df1_p_codfid_main <- df_1_persone %>%
  group_by(LAST_TYP_CLI_FID) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
  arrange(desc(PERCENT))

df1_p_codfid_main #L'ultima tessera fedelt? attivata dai clienti nel 98.4% dei casi
#? una tessera principale (328388 casi), mentre nell'1.6% dei casi
#(5246 casi) abbiamo che la tessera attivata ? secondaria


## plot distribution

plot_df1_p_codfid_main <- (
  ggplot(data=df1_p_codfid_main
         , aes(x=LAST_TYP_CLI_FID, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

ggplotly(plot_df1_p_codfid_main)


### variabile Negozio Online / Negozio Fisico ###

## compute distribution
df1_p_codfid_neg <- df_1_cli_fid_clean %>%
  group_by(RegOnline) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
  arrange(desc(PERCENT))

df1_p_codfid_neg

## plot distribution

plot_df1_p_codfid_neg <- (
  ggplot(data=df1_p_codfid_neg
         , aes(x=RegOnline, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

ggplotly(plot_df1_p_codfid_neg)


#### FINAL REVIEW df_1_clean ####

str(df_1_persone)
summary(df_1_persone)
