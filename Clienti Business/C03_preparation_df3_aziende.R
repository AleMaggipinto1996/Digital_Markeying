#### consideriamo le aziende clienti utilizzando il dataset df_2_aziende ####

id_aziende_indirizzo <- as.data.frame(df_2_aziende$ID_ADDRESS)
colnames(id_aziende_indirizzo) <- "ID_ADDRESS"

df_3_aziende <- merge( df_3_cli_address_clean, id_aziende_indirizzo, by="ID_ADDRESS")

#### EXPLORE COLUMNS of df_3 ####

# EXPLORE the df_3_cli_address_clean relevant variables

### Variabile REGION ###

df_3_aziende_region_distrib <- df_3_aziende %>%
  group_by(REGION) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_aziende_region_distrib

## plot distribution
plot_df_3_aziende_region_distrib <- (
  ggplot(data=df_3_aziende_region_distrib
         , aes(x=REGION, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_aziende_region_distrib

#soprattutto la Lombardia

### variabile PROVINCIA ###

## compute distribution
df_3_aziende_prv_distrib <- df_3_aziende %>%
  group_by(PRV) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_aziende_prv_distrib

## plot distribution
plot_df_3_aziende_prv_distrib <- (
  ggplot(data=df_3_aziende_prv_distrib
         , aes(x=PRV, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_aziende_prv_distrib

# troppe variabili non è significativo

### variabile CAP ###

## compute distribution
df_3_aziende_cap_distrib <- df_3_aziende %>%
  group_by(CAP) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_aziende_cap_distrib

#anche i CAP ci sono troppe variabili non è utile

#### FINAL REVIEW df_3_clean ####

str(df_3_aziende)
summary(df_3_aziende)
