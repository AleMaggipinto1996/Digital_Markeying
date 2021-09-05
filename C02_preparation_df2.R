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

#342167 clienti hanno fornito il proprio numero telefonico (il 92.6%)
#27305 clienti non hanno fornito il loro recapito telefonico (corrispondono al 7.39% dei clienti totali)



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



###########IPOTESI 

### Variabile W_PHONE ###
##Distribuzione dei clienti che possiedono un telefono
##0 se hanno fornito il loro numero telefonico
##1 se lo hanno fornito

dist_df2_wphone <- df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarise(TOT_CLIENTI = n_distinct(ID_CLI)) %>%
  mutate(PERC_CLIENTI = TOT_CLIENTI/sum(TOT_CLIENTI))


plot_dist_df2_wphone <- ggplot(dist_df2_wphone,aes(x=W_PHONE,y=TOT_CLIENTI)) +
  geom_bar(stat = "identity",aes(fill=W_PHONE)) +
  scale_fill_manual(values = c("darkgreen","red"),guide=F)

plot_dist_df2_wphone

### Variabile TYP_CLI_ACCOUNT ###
##Calcoliamo la distribuzione di TYP_CLI_ACCOUNT, che pu?
##assumere valori 2 e 4

dist_df2_tca <- df_2_cli_account_clean %>%
  group_by(TYP_CLI_ACCOUNT) %>%
  summarise(TOT_CLIENTI = n_distinct(ID_CLI)) %>%
  mutate(PERC_CLIENTI = TOT_CLIENTI/sum(TOT_CLIENTI))

dist_df2_tca
#35816 clienti (pari al 9.69%) hanno un account di tipo 2
#333656 clienti (pari al 90.31%) hanno un account di tipo 4

plot_dist_df2_tca <- ggplot(dist_df2_tca,aes(TYP_CLI_ACCOUNT,TOT_CLIENTI)) +
  geom_bar(stat = "identity",fill = "purple")

plot_dist_df2_tca

### Variabile TYP_JOB ###
##Studio della distribuzione dei clienti in base al lavoro che
##svolgono

dist_typ_job <- df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarise(TOT_CLIENTI = n_distinct(ID_CLI)) %>%
  mutate(PERC_CLIENTI = TOT_CLIENTI/sum(TOT_CLIENTI) )

dist_typ_job
#Il 97.7% dei clienti non ha fornito informazioni riguardo alla
#professione che svolge. Tra chi ha fornito indicazioni il lavoro
#piu' svolto e' il libero professionista

plot_dist_typ_job <- ggplot(dist_typ_job,aes(TYP_JOB,TOT_CLIENTI)) +
  geom_bar(stat = "identity",fill="darkred")

plot_dist_typ_job

#### FINAL REVIEW df_2_clean ####

str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)