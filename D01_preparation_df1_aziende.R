
df_1_aziende <- df_1_cli_fid_clean %>%  
  filter(LAST_COD_FID == 'PREMIUM BIZ' | LAST_COD_FID == 'STANDARD BIZ')

#_________________________________________

############# consideriamo la parte che riguarda i clienti business ###########

#_________________________________________


### variable LAST_DT_ACTIVE per MESI ###

## compute distribution 
df1_aziende_codfid_ld <- df_1_aziende %>%
  group_by(substring(LAST_DT_ACTIVE,1,7)) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Mesi = `substring(LAST_DT_ACTIVE, 1, 7)`)

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
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Year = `substring(LAST_DT_ACTIVE, 1, 4)`)

df1_az_codfid_ld


## plot distribution

plot_df1_az_codfid_ld <- (
  ggplot(data=df1_az_codfid_ld
         , aes(x=Year, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

ggplotly(plot_df1_az_codfid_ld)


### variable FIRST_DT_ACTIVE per MESI ###

## compute distribution
df1_az_codfid_fd <- df_1_aziende %>%
  group_by(substring(FIRST_DT_ACTIVE,1,7)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Mesi = `substring(FIRST_DT_ACTIVE, 1, 7)`)

df1_az_codfid_fd

## plot distribution

plot_df1_az_codfid_fd <- (
  ggplot(data=df1_az_codfid_fd
         , aes(x=Mesi, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

ggplotly(plot_df1_az_codfid_fd)



### variable FIRST_DT_ACTIVE per ANNI ###

## compute distribution
df1_az_codfid_fd <- df_1_aziende %>%
  group_by(substring(FIRST_DT_ACTIVE,1,4)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Year = `substring(FIRST_DT_ACTIVE, 1, 4)`)

df1_az_codfid_fd

## plot distribution

plot_df1_az_codfid_fd <- (
  ggplot(data=df1_az_codfid_fd
         , aes(x=Year, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

ggplotly(plot_df1_az_codfid_fd)


### variable LAST_STATUS_FID ###

## compute distribution
df1_az_codfid_status <- df_1_aziende %>%
  group_by(LAST_STATUS_FID) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
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

ggplotly(plot_df1_az_codfid_status)


### variable NUM_FIDS ###

## compute distribution
df1_az_codfid_n <- df_1_aziende%>%
  group_by(NUM_FIDs) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
  arrange(desc(PERCENT))

df1_az_codfid_n

## plot distribution

plot_df1_az_codfid_n <- (
  ggplot(data=df1_az_codfid_n
         , aes(x=NUM_FIDs, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

ggplotly(plot_df1_az_codfid_n)

### variable LAST_TYP_CLI_FID ###

## compute distribution
df1_az_codfid_main <- df_1_aziende %>%
  group_by(LAST_TYP_CLI_FID) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
  arrange(desc(PERCENT))

df1_az_codfid_main

## plot distribution

plot_df1_az_codfid_main <- (
  ggplot(data=df1_az_codfid_main
         , aes(x=LAST_TYP_CLI_FID, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

ggplotly(plot_df1_az_codfid_main)


### variabile Negozio Online / Negozio Fisico ###

## compute distribution
df1_az_codfid_neg <- df_1_aziende %>%
  group_by(RegOnline) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)*100) %>%
  arrange(desc(PERCENT))

df1_az_codfid_neg

## plot distribution

plot_df1_az_codfid_neg <- (
  ggplot(data=df1_az_codfid_neg
         , aes(x=RegOnline, y=PERCENT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

ggplotly(plot_df1_az_codfid_neg)


#### FINAL REVIEW df_1_clean ####

str(df_1_aziende)
summary(df_1_aziende)
