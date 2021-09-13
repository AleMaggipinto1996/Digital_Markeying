##### consideriamo solo le aziende clienti #####

id_aziende <- as.data.frame(df_1_aziende$ID_CLI)
colnames(id_aziende) <- "ID_CLI"

df4_aziende <- merge(id_aziende, df_4_cli_privacy_clean, by="ID_CLI")

#### EXPLORE COLUMNS of df_4 ####

# EXPLORE the df_4_cli_privacy_clean relevant variables

### variabile Privacy 1 ###

## compute distribution
df_4_az_flag1_distrib <- df4_aziende %>%
  group_by(FLAG_PRIVACY_1) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_az_flag1_distrib   #67% non ha dato il consenso e 33% ha dato il consenso

## plot distribution
plot_df_4_az_flag1_distrib <- (
  ggplot(data=df_4_az_flag1_distrib
         , aes(x=FLAG_PRIVACY_1, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_az_flag1_distrib

### Variabile Privacy 2 ###

## compute distribution
df_4_cli_az_flag2_distrib <- df4_aziende %>%
  group_by(FLAG_PRIVACY_2) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_cli_az_flag2_distrib  ##il 99,7% ha dato il consenso

## plot distribution
plot_df_4_az_flag2_distrib <- (
  ggplot(data=df_4_cli_az_flag2_distrib
         , aes(x=FLAG_PRIVACY_2, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_az_flag2_distrib

### variabile direct_marketing ###

## compute distribution
df_4_az_flag_mkt_distrib <- df4_aziende %>%
  group_by(FLAG_DIRECT_MKT) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_az_flag_mkt_distrib #70% non ha dato il consenso e 30% ha dato il consenso

## plot distribution
plot_df_4_az_flag_mkt_distrib <- (
  ggplot(data=df_4_az_flag_mkt_distrib
         , aes(x=FLAG_DIRECT_MKT, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_az_flag_mkt_distrib


#### FINAL REVIEW df_4_clean ####

str(df4_aziende)
summary(df4_aziende)

