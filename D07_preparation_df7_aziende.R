
##### consideriamo solo la categoria aziende #####

df_7_aziende <- merge(id_aziende,df_7_tic_clean_final, by="ID_CLI")

cons_idcli_df1_df7_az <- df_1_cli_fid_clean %>%
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

cons_idcli_df1_df7_az

### ugualmente alcuni clienti mappati in df_1 non lo sono in df_7

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview_az <- df_7_aziende %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview_az

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction_az <- df_7_aziende %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview_az$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview_az$TOT_CLIs)

df7_dist_direction_az


### Come per le persone, molta merce venduta poca rimborsata

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour_az <- df_7_aziende %>%
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

df7_dist_hour_az

## plot aggregate
plot_df7_dist_hour_az <- (
  ggplot(data=df7_dist_hour_az
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

ggplotly(plot_df7_dist_hour_az)

## plot aggregate percent
plot_df7_dist_hour_percent_az <- (
  ggplot(data=df7_dist_hour_az
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

ggplotly(plot_df7_dist_hour_percent_az)


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep_az <- df_7_aziende %>%
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

df7_dist_dep_az

## plot aggregate
plot_df7_dist_dep_az <- (
  ggplot(data=df7_dist_dep_az
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

ggplotly(plot_df7_dist_dep_az)

## plot aggregate percent
plot_df7_dist_dep_percent_az <- (
  ggplot(data=df7_dist_dep_az
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

ggplotly(plot_df7_dist_dep_percent_az)

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp_az <- df_7_aziende %>%
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

df7_dist_datetyp_az

## plot aggregate
plot_df7_dist_datetyp_az <- (
  ggplot(data=df7_dist_datetyp_az
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

ggplotly(plot_df7_dist_datetyp_az)

## plot aggregate percent
plot_df7_dist_datetyp_percent_az <- (
  ggplot(data=df7_dist_datetyp_az
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

ggplotly(plot_df7_dist_datetyp_percent_az)

## gli acquisti e i rimborsi si suddividono in maniera equa

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###
## compute aggregate
df7_dist_importosconto_az <- df_7_aziende %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_az <- df7_dist_importosconto_az %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_az

#L'importo lordo medio degli acquisti è pari a 187 con uno sconto di 12.9

## plot aggregate
plot_df7_dist_importo_az <- (
  ggplot(data=df7_dist_importosconto_az %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

ggplotly(plot_df7_dist_importo_az)
#All'aumentare dei prezzi gli acquisti ed i resi decrescono esponenzialmente

## plot aggregate
plot_df7_dist_sconto_az <- (
  ggplot(data=df7_dist_importosconto_az %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

ggplotly(plot_df7_dist_sconto_az)

# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO

df7_avgil_az <- df_7_aziende %>% group_by(COD_REPARTO) %>%
  summarise(IMPORTO_MEDIO = mean(IMPORTO_LORDO))
df7_avgil_az

df7_avgil_az[which.max(df7_avgil_az$IMPORTO_MEDIO),]
df7_avgil_az[which.min(df7_avgil_az$IMPORTO_MEDIO),]


df7_avgsc_az <- df_7_aziende %>% filter(DIREZIONE==1)  %>% group_by(COD_REPARTO) %>%
  summarise(SCONTO_MEDIO = mean(SCONTO))

df7_avgsc_az[which.max(df7_avgsc_az$SCONTO_MEDIO),]
df7_avgsc_az[which.min(df7_avgsc_az$SCONTO_MEDIO),]

IL_az<-ggplot(df7_avgil_az,aes(x=COD_REPARTO,y=IMPORTO_MEDIO))+
  geom_bar(stat = "identity",fill="green")+
  theme_minimal()
ggplotly(IL_az)

SC_az<-ggplot(df7_avgsc_az,aes(x=COD_REPARTO,y=SCONTO_MEDIO))+
  geom_bar(stat = "identity",fill="blue")+
  theme_minimal()
ggplotly(SC_az)


# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

# number of tics per articolo
df7_dist_tics_articolo_az <- df_7_aziende %>%
  group_by(ID_ARTICOLO) %>%
  dplyr::summarize(NUM_TICs = sum(n_distinct(ID_SCONTRINO))) %>%
  ungroup()

df7_dist_tics_articolo_az

# distribution of TICs number    ## quante volte sono stati acquistati tot volte articoli diversi
df7_dist_numtics_articolo_az <- df7_dist_tics_articolo_az %>%
  group_by(NUM_TICs) %>%
  dplyr::summarize(COUNT_ART = sum(n_distinct(ID_ARTICOLO))) %>%
  ungroup()

df7_dist_numtics_articolo_az

# plot aggregate
plot_df7_dist_numtics_articolo_az <- df7_dist_numtics_articolo_az %>%
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

ggplotly(plot_df7_dist_numtics_articolo_az)

# ARTICOLO WITH MAX num TICs  
df7_articolo_max_tics_az<-df_7_aziende %>% group_by(ID_ARTICOLO) %>%
  summarise(TOT_SCONTRINI = n_distinct(ID_SCONTRINO),
            TOT_CLIENTI = n_distinct(ID_CLI))
df7_articolo_max_tics_az[which.max(df7_articolo_max_tics_az$TOT_SCONTRINI),]

#L'articolo con ID 33700716 è acquistato da 4789 clienti diversi 9197 volte 


# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

## compute aggregate
df7_dist_importosconto_cli_az <- df_7_aziende %>%
  group_by(ID_CLI, DIREZIONE) %>%
  dplyr::summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
                   , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_cli_az <- df7_dist_importosconto_cli_az %>%
  group_by(DIREZIONE) %>%
  dplyr::summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
                   , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_cli_az

## plot aggregate
plot_df7_dist_importo_cli_az <- (
  ggplot(data=df7_dist_importosconto_cli_az %>%
           filter()
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

ggplotly(plot_df7_dist_importo_cli_az)

## plot aggregate
plot_df7_dist_sconto_cli_az <- (
  ggplot(data=df7_dist_importosconto_cli_az %>%
           filter()
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

ggplotly(plot_df7_dist_sconto_cli_az)


# compute the distribution of customers by number of purchases (as described in the slides)

df7_purch_az <- df_7_aziende %>% filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(TOT_ACQUISTI = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(TOT_ACQUISTI))

df7_sub_purch_az <- df7_purch_az %>%
  mutate(CATEGORIA = case_when((TOT_ACQUISTI>=1) & (TOT_ACQUISTI<10) ~ "meno di 10",
                               (TOT_ACQUISTI>=10) & (TOT_ACQUISTI<25) ~ "da 10 a 25",
                               (TOT_ACQUISTI>=25) & (TOT_ACQUISTI<50) ~ "da 25 a 50",
                               (TOT_ACQUISTI>=50) & (TOT_ACQUISTI<100) ~ "da 50 a 100",
                               (TOT_ACQUISTI>=100) ~ "più di 100"))

df7_sub_purch_az <- df7_sub_purch_az %>%
  mutate(CAT = factor(CATEGORIA,levels=c("meno di 10","da 10 a 25",
                                         "da 25 a 50","da 50 a 100",
                                         "più di 100"))) %>%
  group_by(CAT) %>%
  summarise(TOT = n_distinct(ID_CLI)) %>%
  mutate(PERC_CLIENTI = TOT/sum(TOT),
         CUM_PERC_CLIENTI = cumsum(PERC_CLIENTI))

df7_sub_purch_az


plot_df7_sub_purch_az <- ggplot(df7_sub_purch_az,aes(CAT,TOT))+
  geom_bar(fill="red4",stat = "identity",width=0.4)+
  xlab("N acquisti")+
  ylab("N clienti")

plot_df7_sub_purch_az

# compute the days for next purchase curve (as described in the slides)

df_for_next_purchase_curve_az <- df_7_aziende %>%
  filter(DIREZIONE == 1) %>% 
  select(ID_CLI,
         ID_ARTICOLO,
         TIC_DATE,
         DIREZIONE)      %>%
  arrange(ID_CLI)

df_for_next_purchase_curve_az


df_date_diff_az <- df_for_next_purchase_curve_az %>%
  group_by(ID_CLI) %>%
  mutate(Days_difference = TIC_DATE - lag(TIC_DATE))
df_date_diff_az

df_days_curve_az <- as.data.frame(table(df_date_diff_az$Days_difference))
colnames(df_days_curve) <- c("Days_diff","Freq")
df_days_curve <- df_days_curve[-1, ]
df_days_curve$Perc <- df_days_curve$Freq/sum(df_days_curve$Freq)

df_days_curve 


#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final_az)
summary(df_7_tic_clean_final_az)


