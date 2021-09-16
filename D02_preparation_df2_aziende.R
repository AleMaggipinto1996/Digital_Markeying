
df_2_aziende <- df_2_cli_account_clean %>%  
  filter(TYP_CLI_ACCOUNT == 2)

#### continuo l'analisi solo sui clienti business

cons_idcli_df1_df2_aziende <- df_1_aziende %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_aziende %>%
              select(ID_CLI) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2_aziende

###### cancello il numero di riferimento di ogni personal_mail_provider per
###### poter creare successivamente una sola categoria di personal_mail_provider
###### (ovvero raggruppare tutte le personal mail provider)

df_2_aziende<-df_2_aziende %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER))

df_2_aziende= df_2_aziende %>% 
  mutate(EMAIL_PROVIDER = replace(EMAIL_PROVIDER, str_detect(EMAIL_PROVIDER, "personal_mail_provider_*"), "personal_mail_provider"))

#### EXPLORE COLUMNS of df_2_aziende ####

### Variable EMAIL_PROVIDER ###

## compute distribution
df_2_az_emailprovider <- df_2_aziende %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_az_emailprovider   #il 2.4% delle aziende clienti non fornisce l'email

tot_emailproviders <- n_distinct(df_2_az_emailprovider$EMAIL_PROVIDER)

tot_emailproviders

#!!! NOTE: too many different values for EMAIL_PROVIDER to be an useful category !!!#
## ci sono email_provider che sono errori di battitura evidenti -> ci sono alcuni
  ##clienti che inseriscono l'email in modo sbagliato 
  ## (non rendendo possibili le comunicazioni marketing)

#### EMAIL_PROVIDER ####
#______________________________________
##mantieni i valori EMAIL_PROVIDER più frequenti e 
##aggiungi un livello di fattore comune "ALTRO" per i restanti

df_2_az_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  as.data.frame() %>%
  head(20)
  

## always keep the (missing) level for technical reasons
## select levels that cover the 85% of the cases, the remaining 15% 
clean_email_providers_az <- df_2_az_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers_az, 20)

## add clean EMAIL_PROVIDER_az ##
df_2_aziende <- df_2_aziende %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers_az %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))


#### EXPLORE NEW COLUMNS EMAIL_PROVIDER_CLEAN in df_2 ####

## compute distribution
df2_az_emailproviderclean <- df_2_aziende %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_az_emailproviderclean

## plot distribution
plot_df2_az_emailproviderclean <- (
  ggplot(data=df2_az_emailproviderclean
         , aes(x=EMAIL_PROVIDER_CLEAN, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df2_az_emailproviderclean
#è usata soprattutto @gmail; 
#la categoria personal_mail_providers rappresenta la 2a piu frequente (27.7%)
#others il 13% (in questa ci sono molti errori di battitura dei clienti)

#_____________________________________________________

# EXPLORE the remaining relevant variables


### variabile occupazione cliente ###

df_2_az_typJob <- df_2_aziende %>%
  group_by(TYP_JOB) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  as.data.frame()

df_2_az_typJob

# tutte le aziende clienti non hanno inserito nulla in questa voce 
# uniamo non dichiarati con quelli mancanti ##############NON C'è BISOGNO DI FARE QUESTA OPERAZIONE###


### variabile W_PHONE ###

## compute distribution
df_2_az_phone <- df_2_aziende %>%
  group_by(W_PHONE) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_az_phone

#31059 aziende clienti hanno fornito il proprio numero telefonico (il 86.7%)
#4757 aziende clienti non hanno fornito il loro recapito telefonico (corrispondono al 13.3% )



## plot distribution

plot_df_2_az_phone <- (
  ggplot(data=df_2_az_phone
         , aes(x=W_PHONE, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_2_az_phone


## compute distribution
df_2_az_type <- df_2_aziende %>%
  group_by(TYP_CLI_ACCOUNT) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_az_type  

## plot distribution

plot_df_2_az_type <- (
  ggplot(data=df_2_az_type
         , aes(x=TYP_CLI_ACCOUNT, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_2_az_type

#### FINAL REVIEW df_2_clean ####

str(df_2_aziende)
summary(df_2_aziende)

