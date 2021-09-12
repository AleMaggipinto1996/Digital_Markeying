#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
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

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )

## aggiunto se è weekend, festivo, giorno della settimana, altro

##### consideriamo solo la categoria persone #####

id_persone <- as.data.frame(df_1_persone$ID_CLI)
colnames(id_persone) <- "ID_CLI"

df_7_persone <- merge(id_persone,df_7_tic_clean_final, by="ID_CLI")

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
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

cons_idcli_df1_df7

### ugualmente alcuni clienti mappati in df_1 non lo sono in df_7

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_persone %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_persone %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction


### molta merce venduta poca rimborsata

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_persone %>%
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

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

ggplotly(plot_df7_dist_hour)

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

ggplotly(plot_df7_dist_hour_percent)


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_persone %>%
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

df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

ggplotly(plot_df7_dist_dep)

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

ggplotly(plot_df7_dist_dep_percent)

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_persone %>%
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

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

ggplotly(plot_df7_dist_datetyp)

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

ggplotly(plot_df7_dist_datetyp_percent)

## gli acquisti vengono fatti soprattutto nei giorni lavorativi poi nel weekend e poco nei giorni festivi
## forse perchè essendo negozi fisici sono chiusi. I rimborsi si suddividono in maniera equa

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###
## compute aggregate
df7_dist_importosconto <- df_7_persone %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

#L'importo lordo medio degli acquisti è pari a 164 con uno sconto di 11.8

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

ggplotly(plot_df7_dist_importo)
#Prevalgono soprattutto gli acquisti e i resi di basso valore economico
#All'aumentare dei prezzi gli acquisti ed i resi decrescono esponenzialmente

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

ggplotly(plot_df7_dist_sconto)

# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO

## compute aggregate
df7_dist_importosconto_reparto <- df_7_persone %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  dplyr::summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
                   , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_reparto <- df7_dist_importosconto_reparto %>%
  group_by(DIREZIONE) %>%
  dplyr::summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
                   , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_reparto

## plot aggregate
plot_df7_dist_importo_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto %>%
           filter()
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo_reparto

## plot aggregate
plot_df7_dist_sconto_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto %>%
           filter()
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto_reparto


# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

# number of tics per articolo
df7_dist_tics_articolo <- df_7_persone %>%
  group_by(ID_ARTICOLO) %>%
  dplyr::summarize(NUM_TICs = sum(n_distinct(ID_SCONTRINO))) %>%
  ungroup()

df7_dist_tics_articolo

# distribution of TICs number    ## quante volte sono stati acquistati tot volte articoli diversi
df7_dist_numtics_articolo <- df7_dist_tics_articolo %>%
  group_by(NUM_TICs) %>%
  dplyr::summarize(COUNT_ART = sum(n_distinct(ID_ARTICOLO))) %>%
  ungroup()

df7_dist_numtics_articolo

# plot aggregate
plot_df7_dist_numtics_articolo <- df7_dist_numtics_articolo %>%
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

ggplotly(plot_df7_dist_numtics_articolo)

# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

## compute aggregate
df7_dist_importosconto_cli <- df_7_persone %>%
  group_by(ID_CLI, DIREZIONE) %>%
  dplyr::summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
                   , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_cli <- df7_dist_importosconto_cli %>%
  group_by(DIREZIONE) %>%
  dplyr::summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
                   , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_cli

## plot aggregate
plot_df7_dist_importo_cli <- (
  ggplot(data=df7_dist_importosconto_cli %>%
           filter()
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

ggplotly(plot_df7_dist_importo_cli)

## plot aggregate
plot_df7_dist_sconto_cli <- (
  ggplot(data=df7_dist_importosconto_cli %>%
           filter()
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

ggplotly(plot_df7_dist_sconto_cli)


# compute the distribution of customers by number of purchases (as described in the slides)

df7_dist_total_purch <- df_7_persone %>%
  filter(DIREZIONE == 1)                             %>% 
  group_by(ID_CLI)                                   %>% 
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>% 
  arrange(desc(TOT_PURCHASE))                           

df7_dist_total_purch


# compute the days for next purchase curve (as described in the slides)

df_for_next_purchase_curve <- df_7_persone %>%
  filter(DIREZIONE == 1) %>% 
  select(ID_CLI,
         ID_ARTICOLO,
         TIC_DATE,
         DIREZIONE)      %>%
  arrange(ID_CLI)

df_for_next_purchase_curve


df_date_diff <- df_for_next_purchase_curve %>%
  group_by(ID_CLI) %>%
  mutate(Days_difference = TIC_DATE - lag(TIC_DATE))

df_date_diff

df_days_curve <- as.data.frame(table(df_date_diff$Days_difference))
colnames(df_days_curve) <- c("Days_diff","Freq")
df_days_curve <- df_days_curve[-1, ]
df_days_curve$Perc <- df_days_curve$Freq/sum(df_days_curve$Freq)

df_days_curve


#### FINAL REVIEW df_7_clean ####

str(df_7_persone)
summary(df_7_persone)




############################## PROPOSTA ########################

# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO

df7_dist_avgimprep <- df_7_tic_clean %>% group_by(COD_REPARTO) %>%
  summarise(IMPORTO_MEDIO = mean(IMPORTO_LORDO))
df7_dist_avgimprep

df7_dist_avgimprep[which.max(df7_dist_avgimprep$IMPORTO_MEDIO),]
df7_dist_avgimprep[which.min(df7_dist_avgimprep$IMPORTO_MEDIO),]


df7_dist_avgscntrep <- df_7_tic_clean %>% filter(DIREZIONE==1)  %>% group_by(COD_REPARTO) %>%
  summarise(SCONTO_MEDIO = mean(SCONTO))

df7_dist_avgscntrep[which.max(df7_dist_avgscntrep$SCONTO_MEDIO),]
df7_dist_avgscntrep[which.min(df7_dist_avgscntrep$SCONTO_MEDIO),]



ggplot(df7_dist_avgimprep,aes(x=COD_REPARTO,y=IMPORTO_MEDIO))+
  geom_bar(stat = "identity",fill="coral")+
  theme_minimal()

ggplot(df7_dist_avgscntrep,aes(x=COD_REPARTO,y=SCONTO_MEDIO))+
  geom_bar(stat = "identity",fill="lightgreen")+
  theme_minimal()

# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)
df7_dist_articolo<-df_7_tic_clean_final %>% group_by(ID_ARTICOLO,DIREZIONE) %>%
  summarise(TOT_SCONTRINI = n_distinct(ID_SCONTRINO),
            TOT_CLIENTI = n_distinct(ID_CLI))
df7_dist_articolo[which.max(df7_dist_articolo$TOT_SCONTRINI),]
#L'articolo pi? acquistato ? il 33700716, acquistato 57806 volte
#da 36273 clienti diversi


df7_dist_articolo

ggplot(df7_dist_articolo,aes(x=ID_ARTICOLO,y=TOT_SCONTRINI))+
  geom_bar(stat = "identity")

# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI
df7_avgimpscntcli <- df_7_tic_clean_final %>%
  group_by(ID_CLI,DIREZIONE) %>%
  summarise(IMPORTO_MEDIO = mean(IMPORTO_LORDO),
            SCONTO_MEDIO = mean(SCONTO))

df7_avgimpscntcli

plot_df7_avgimpcli <- ggplot(df7_avgimpscntcli,aes(x=ID_CLI,y=IMPORTO_MEDIO))+
  geom_bar(stat = "identity")
plot_df7_avgimpcli

plot_df7_avgscntcli <- ggplot(df7_avgimpscntcli,aes(x=ID_CLI,y=SCONTO_MEDIO))+ #non va
  geom_bar(stat = "identity")
plot_df7_avgscntcli

# compute the distribution of customers by number of purchases (as described in the slides)
df7_dist_purch <- df_7_tic_clean_final %>% filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(TOT_ACQUISTI = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(TOT_ACQUISTI))

df7_dist_purch_cat <- df7_dist_purch %>%
  mutate(CATEGORIA = case_when((TOT_ACQUISTI>=1) & (TOT_ACQUISTI<5) ~ "meno di 5",
                               (TOT_ACQUISTI>=5) & (TOT_ACQUISTI<20) ~ "dai 5 ai 20",
                               (TOT_ACQUISTI>=20) & (TOT_ACQUISTI<50) ~ "dai 20 ai 50",
                               (TOT_ACQUISTI>=50) & (TOT_ACQUISTI<100) ~ "dai 50 ai 100",
                               (TOT_ACQUISTI>=100) ~ "pi? di 100"))

df7_dist_purch_cat <- df7_dist_purch_cat %>%
  mutate(CAT = factor(CATEGORIA,levels=c("meno di 5","dai 5 ai 20",
                                         "dai 20 ai 50","dai 50 ai 100",
                                         "pi? di 100"))) %>%
  group_by(CAT) %>%
  summarise(TOT = n_distinct(ID_CLI)) %>%
  mutate(PERC_CLIENTI = TOT/sum(TOT),
         CUM_PERC_CLIENTI = cumsum(PERC_CLIENTI))

df7_dist_purch_cat


plot_df7_dist_purch_cat <- ggplot(df7_dist_purch_cat,aes(CAT,TOT))+
  geom_bar(fill="darkblue",stat = "identity",width=0.4)+
  xlab("Numero di acquisti")+
  ylab("Numero di clienti")

plot_df7_dist_purch_cat

# compute the days for next purchase curve (as described in the slides)
df7_nxtpurch <- df_7_tic_clean_final %>% filter(DIREZIONE==1) %>%
  select(ID_CLI,TIC_DATE) %>%
  unique()

df7_nxtpurch2 <- df7_nxtpurch %>% group_by(ID_CLI) %>%
  summarise(AVG_PURCH_DIFF = round(mean(diff(TIC_DATE))),
            LAST_PURCH = nth(TIC_DATE,1),
            SECOND_LAST = nth(TIC_DATE,2))

#Omitting the NA values
df7_nxtpurch3 <- df7_nxtpurch2 %>% mutate(AVG_PURCH_DIFF = as.numeric(AVG_PURCH_DIFF))
df7_nxtpurch3 <- df7_nxtpurch3[complete.cases(df7_nxtpurch3),]

df7_nxtpurch4 <- df7_nxtpurch3 %>% group_by(AVG_PURCH_DIFF) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(ALL_CLIs = sum(TOT_CLIs),
         CUM_CLIs = cumsum(TOT_CLIs))

df7_nxtpurch4 <- df7_nxtpurch4 %>% mutate(
  PERC_CLIS = TOT_CLIs/ALL_CLIs,
  CUM_PERC_CLIs = CUM_CLIs/ALL_CLIs
)

df7_nxtpurch4[which(df7_nxtpurch4$CUM_PERC_CLIs > 0.8)[1],]
#The 80% of the customers repurchase within 61 days

plot_df7_nxtpurch <- ggplot(df7_nxtpurch4,aes(AVG_PURCH_DIFF,CUM_PERC_CLIs))+
  geom_line(stat = "identity",size=1.4) +
  geom_vline(xintercept=61, linetype="dashed", color = "red", size = 0.5)+
  geom_hline(yintercept=0.8, linetype="dashed", color = "red", size = 0.5)+
  geom_text(aes(x=61, label="80%", y=0.75), colour="#404040", hjust = -0.5)

plot_df7_nxtpurch
#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)


