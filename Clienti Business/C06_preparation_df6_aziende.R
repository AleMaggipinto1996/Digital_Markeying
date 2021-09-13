##### consideriamo solo le aziende clienti ######

df_6_aziende <- merge( id_aziende,df_6_camp_event_clean_final, by="ID_CLI")

cons_idcli_df1_df6_az <- df_1_aziende %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_6_aziende %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_6) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df6_az

#### anche qui tutte quelle in df_6 sono mappate in df_1, ma non tutte quelle di df_1 sono in df_6

#### EXPLORE VARIABLES in df_6 ####

### GENERAL OVERVIEW ###

## compute aggregate
df6_overview_az <- df_6_aziende %>% 
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overview_az

### GENERAL OVERVIEW by TYP_CAMP ###

## compute aggregate
df6_az_overviewbytyp <- df_6_aziende %>%
  group_by(TYP_CAMP) %>%
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_az_overviewbytyp

## plot aggregate
plot_df6_az_overviewbytyp <- (
  ggplot(data=df6_az_overviewbytyp
         , aes(x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_az_overviewbytyp

###sono state fatte soprattutto campagne nazionali

### Variable OPENED ###

## compute aggregate
df6_az_dist_opened <- df_6_aziende %>%
  group_by(OPENED) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview_az$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview_az$TOT_CLIs)

df6_az_dist_opened

## plot aggregate
plot_df6_az_dist_opened <- (
  ggplot(data=df6_az_dist_opened
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    theme_minimal()
)

plot_df6_az_dist_opened

### per la maggior parte non sono state aperte

### Variable OPENED by TYP_CAMP ###

## compute aggregate
df6_az_dist_openedbytyp <- df_6_aziende %>%
  group_by(TYP_CAMP, OPENED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_az_overviewbytyp %>%
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

df6_az_dist_openedbytyp

## plot aggregate
plot_df6_az_dist_openedbytyp <- (
  ggplot(data=df6_az_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_az_dist_openedbytyp

## plot aggregate percent
plot_df6_az_dist_openedbytyp_percent <- (
  ggplot(data=df6_az_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_az_dist_openedbytyp_percent

#### sono state aperte di più quelle riferite al prodotto, poi
### quelle personalizzare, quelle locali e infine quelle nazionali

### Variable DAYS_TO_OPEN

## compute aggregate
df6_az_dist_daystoopen <- df_6_aziende %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarize(AVG_DAYS_TO_OPEN = floor(mean(DAYS_TO_OPEN))) %>%
  ungroup() %>%
  group_by(AVG_DAYS_TO_OPEN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df6_az_dist_daystoopen

## plot aggregate
plot_df6_az_dist_daystoopen <- (
  ggplot(data=df6_az_dist_daystoopen %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_az_dist_daystoopen

#### la maggior parte vengono aperte subito o il giorno dopo
##più passano i giorni più diminuiscono

### DAYS_TO_OPEN vs CUMULATE PERCENT ### 

## compute aggregate
df6_az_dist_daystoopen_vs_cumulate <- df6_az_dist_daystoopen %>%
  arrange(AVG_DAYS_TO_OPEN) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

## plot aggregate
plot_df6_az_dist_daystoopen_vs_cumulate <- (
  ggplot(data=df6_az_dist_daystoopen_vs_cumulate %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=PERCENT_COVERED)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) +
    theme_minimal()
)

plot_df6_az_dist_daystoopen_vs_cumulate

# EXPLORE the following relevant variables in df_6_camp_event_clean_final:

# - CLICKED/CLICKED by TYP_CAMP

df6_az_dist_clickedbytyp <- df_6_aziende %>%
  group_by(TYP_CAMP, CLICKED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_az_overviewbytyp %>%
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

df6_az_dist_clickedbytyp

## plot aggregate
plot_df6_az_dist_clickedbytyp <- (
  ggplot(data=df6_az_dist_clickedbytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_az_dist_clickedbytyp

## plot aggregate percent
plot_df6_az_dist_clickedbytyp_percent <- (
  ggplot(data=df6_az_dist_clickedbytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_az_dist_clickedbytyp_percent

# in generale pochissimi click
#più sui prodotti e su quelli nazionali


# - FAILED/FAILED by TYP_CAP

## compute aggregate
df6_az_dist_failedbytyp <- df_6_aziende %>%
  group_by(TYP_CAMP, FAILED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_az_overviewbytyp %>%
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

df6_az_dist_failedbytyp

## plot aggregate
plot_df6_az_dist_failedbytyp <- (
  ggplot(data=df6_az_dist_failedbytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_az_dist_failedbytyp

## plot aggregate percent
plot_df6_az_dist_failedbytyp_percent <- (
  ggplot(data=df6_az_dist_failedbytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_az_dist_failedbytyp_percent

# pochi eventi falliti, ma soprattutto quelli nazionali

# - NUM_OPENs

# compute aggregate
df6_az_dist_num_opens <- df_6_aziende %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarize(AVG_OPENs = floor(mean(NUM_OPENs))) %>%
  ungroup() %>%
  group_by(AVG_OPENs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df6_az_dist_num_opens 

## plot 
plot_df6_az_dist_num_opens <- (
  ggplot(data=df6_az_dist_num_opens
         , aes(x=AVG_OPENs, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_az_dist_num_opens

#pochi clienti riaprono più di una volta, molto pochi più di due

# - NUM_CLICKs

# compute aggregate
df6_az_num_clicks <- df_6_aziende %>%
  filter(CLICKED) %>%
  group_by(ID_CLI) %>%
  summarize(AVG_CLICKs = floor(sum(NUM_CLICKs))) %>%
  ungroup() 
df6_az_dist_num_clicks <- df6_az_num_clicks %>%
  group_by(AVG_CLICKs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df6_az_dist_num_clicks

## plot 
plot_df6_az_dist_num_clicks <- (
  ggplot(data=df6_az_dist_num_clicks
         , aes(x=AVG_CLICKs, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_az_dist_num_clicks

# per i click sono stati cliccati diverse volte, ma principalemnte sempre una volta


#### FINAL REVIEW df_6_clean ####

str(df_6_aziende)
summary(df_6_aziende)





