
############

# MODELLO RFM per la categoria aziende

################

lastdate2 = df_7_aziende %>% summarise(max(TIC_DATE))
lastdate2=lastdate2[1,1]
## prima data di acquisto risale al 2018-05-01, l'ultima data di acquisto risale al 2019-04-30

#vogliamo trovare i clienti attivi (80%)

#troviamo l'ultimo giorno di acquisto per ogni cliente
#e quanti giorni sono passati dall'ultimo acquisto in generale

df_last_date_aziende <- df_7_aziende %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_aziende)
boxplot(df_last_date_aziende$DIFF_DAYS)

giorni_max=quantile(df_last_date_aziende$DIFF_DAYS, probs = c(0.80, 0.85, 0.90))
giorni_max

#decidiamo di considerare attivi l'ottanta percento dei clienti

giorni_max = giorni_max[1]

aziende_attive <- df_last_date_aziende %>% filter(DIFF_DAYS<=giorni_max)

df_aziende_attive <- merge( df_7_aziende, aziende_attive, by="ID_CLI")

################### MODELLO RFM DAL 2018/08/01 AL 2019/01/31 #######################

df_aziende_attive1 <- df_aziende_attive %>% 
  filter(TIC_DATE <= as.Date("2019-01-31") & TIC_DATE >= as.Date("2018-08-01")) %>%
  select(-LAST_DATE_PURCH, - DIFF_DAYS)

df_last_date_aziende_1 <- df_aziende_attive1 %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_aziende_1)
boxplot(df_last_date_aziende_1$DIFF_DAYS)


df_aziende_attive_1 <- merge(df_aziende_attive1, df_last_date_aziende_1, by="ID_CLI")

##Calcolo Recency: l'ultimo acquisto dopo quanto tempo

quantili<- quantile(df_last_date_aziende_1$DIFF_DAYS, probs = c(0.25, 0.50, 0.75))
quantili
#25% 50% 75% 
#108 141 176 

Recency_aziende_1 = df_last_date_aziende_1 %>% mutate(CLASS_R=case_when(DIFF_DAYS<quantili[1] ~ "Low",
                                                                        (DIFF_DAYS>=quantili[1]) & (DIFF_DAYS<quantili[3])~"Medium",
                                                                        (DIFF_DAYS>=quantili[3])~"High"))


Recency_aziende_1 = mutate(Recency_aziende_1,CLASS_R=factor(CLASS_R,levels=c("Low","Medium","High")))


##Calcolo Frequency: ogni quanto acquisto

Frequency_aziende_1 <- df_aziende_attive_1 %>%
  filter(DIREZIONE==1) %>% group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>0)

boxplot(Frequency_aziende_1$N_SCONTRINI_PER_CLI)

quantili<- quantile(Frequency_aziende_1$N_SCONTRINI_PER_CLI,probs = c(0.50,0.70,0.90))
quantili
#50% 70% 90% 
#2   4   9 

Frequency_aziende_1 <- Frequency_aziende_1 %>% mutate(CLASS_F=case_when(N_SCONTRINI_PER_CLI<quantili[1] ~ "Low",
                                                                        (N_SCONTRINI_PER_CLI>=quantili[1]) & (N_SCONTRINI_PER_CLI<quantili[3])~"Medium",
                                                                        (N_SCONTRINI_PER_CLI>=quantili[3])~"High"))
Frequency_aziende_1 = mutate(Frequency_aziende_1,CLASS_F=factor(CLASS_F,levels=c("Low","Medium","High")))

#Monetary Value (differenza tra importo lordo e sconto)

Monetary_Value_aziende_1 = df_aziende_attive_1 %>% filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(AMOUNT = sum(IMPORTO_LORDO) - sum(SCONTO))

quantili = quantile(Monetary_Value_aziende_1$AMOUNT,probs = c(0.25,0.50,0.75))
quantili
#25%     50%     75% 
#89.130 251.985 689.870 

Monetary_Value_aziende_1 = Monetary_Value_aziende_1 %>% mutate(CLASS_M = case_when(AMOUNT<quantili[1]~"Low",
                                                                                   (AMOUNT>=quantili[1])&(AMOUNT<quantili[3])~"Medium",
                                                                                   AMOUNT>=quantili[3]~"High"))
Monetary_Value_aziende_1 = mutate(Monetary_Value_aziende_1,CLASS_M=factor(CLASS_M,levels=c("Low","Medium","High")))

# Recency-Frequency

RF_aziende_1 <- merge(Recency_aziende_1, Frequency_aziende_1,by="ID_CLI")
RF_aziende_1 <- RF_aziende_1 %>% mutate(CLASS_RF_aziende_1 = case_when((CLASS_F=="Low")&(CLASS_R=="Low")~"One-Timer",
                                                                       (CLASS_F=="Low")&(CLASS_R=="Medium")~"One-Timer",
                                                                       (CLASS_F=="Low")&(CLASS_R=="High")~"Leaving",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="Low")~"Engaged",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="Medium")~"Engaged",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="High")~"Leaving",
                                                                       (CLASS_F=="High")&(CLASS_R=="Low")~"Top",
                                                                       (CLASS_F=="High")&(CLASS_R=="Medium")~"Top",
                                                                       (CLASS_F=="High")&(CLASS_R=="High")~"Leaving Top"))

Fedelta_aziende_1 <- as.data.frame(with(RF_aziende_1,table(CLASS_RF_aziende_1)))
Fedelta_aziende_1 <- Fedelta_aziende_1 %>% mutate(CLASS_RF_aziende_1 = factor(CLASS_RF_aziende_1,levels = c("Leaving","One-Timer",
                                                                                                            "Engaged","Leaving Top",
                                                                                                            "Top")))
Fedelta_aziende_1
summary(Fedelta_aziende_1)

#per la maggior parte sono Engaged

Fedelta_aziende_1$Freq <- (Fedelta_aziende_1$Freq / nrow(RF_aziende_1))

Fedelta_aziende_1_plot <- 
  ggplot(Fedelta_aziende_1,aes(CLASS_RF_aziende_1,Freq,fill=CLASS_RF_aziende_1)) + geom_bar(stat = "identity",width = 0.5)+
  ggtitle("Fedeltà")+ylab("Percentuale di Clienti")+xlab("Fedeltà")+
  scale_fill_brewer(palette = "Reds")+ scale_y_continuous(labels = percent) +
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

Fedelta_aziende_1_plot


##### RFM_aziende_1

RFM_aziende_1 <- RF_aziende_1 %>% select(-c("LAST_DATE_PURCH","CLASS_R","CLASS_F")) %>% 
  left_join(Monetary_Value_aziende_1,by="ID_CLI")
RFM_aziende_1 <- mutate(RFM_aziende_1,CLASS_RF_aziende_1=factor(CLASS_RF_aziende_1))


RFM_aziende_1 <- RFM_aziende_1 %>% mutate(CLASSI_aziende_1 = case_when((CLASS_M=="Low") & (CLASS_RF_aziende_1=="One-Timer")~"Cheap",
                                                                       (CLASS_M=="Low") & (CLASS_RF_aziende_1=="Leaving")~"Tin",
                                                                       (CLASS_M=="Low") & (CLASS_RF_aziende_1=="Engaged")~"Copper",
                                                                       (CLASS_M=="Low") & (CLASS_RF_aziende_1=="Leaving Top")~"Bronze",
                                                                       (CLASS_M=="Low") & (CLASS_RF_aziende_1=="Top")~"Silver",
                                                                       (CLASS_M=="Medium") & (CLASS_RF_aziende_1=="One-Timer")~"Tin",
                                                                       (CLASS_M=="Medium") & (CLASS_RF_aziende_1=="Leaving")~"Copper",
                                                                       (CLASS_M=="Medium") & (CLASS_RF_aziende_1=="Engaged")~"Bronze",
                                                                       (CLASS_M=="Medium") & (CLASS_RF_aziende_1=="Leaving Top")~"Silver",
                                                                       (CLASS_M=="Medium") & (CLASS_RF_aziende_1=="Top")~"Gold",
                                                                       (CLASS_M=="High") & (CLASS_RF_aziende_1=="One-Timer")~"Copper",
                                                                       (CLASS_M=="High") & (CLASS_RF_aziende_1=="Leaving")~"Bronze",
                                                                       (CLASS_M=="High") & (CLASS_RF_aziende_1=="Engaged")~"Silver",
                                                                       (CLASS_M=="High") & (CLASS_RF_aziende_1=="Leaving Top")~"Gold",
                                                                       (CLASS_M=="High") & (CLASS_RF_aziende_1=="Top")~"Diamond"))
RFM_aziende_1 = RFM_aziende_1 %>% mutate(CLASSI_aziende_1 = factor(CLASSI_aziende_1,levels = c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")))
RFM_TOT_aziende_1 <- as.data.frame(with(RFM_aziende_1,table(CLASSI_aziende_1)))

RFM_TOT_aziende_1 #la maggior parte dei clienti appartengono alla categoria Bronze e TIN e Copper 

RFM_TOT_aziende_1$Freq <- (RFM_TOT_aziende_1$Freq / nrow(RFM_aziende_1))


#EXPLORATORY ANALYSIS of RFM's dataframe
RFM_aziende_1_plot <- 
  ggplot(RFM_TOT_aziende_1,aes(CLASSI_aziende_1,Freq,fill=CLASSI_aziende_1)) + geom_bar(stat = "identity")+
  labs(title = "Customer's distribution",size=18)+ylab("Percentuale clienti")+ scale_y_continuous(labels = percent)+
  scale_fill_manual(values=c("black","#2F4F4F","#801818","#CD7F32","#C0C0C0","gold","#B0E0E6"),guide=F)+
  theme_minimal()

RFM_aziende_1_plot


################ MODELLO RFM DAL 2018/11/01 AL 2019/04/30 ########################

df_aziende_attive2 <- df_aziende_attive %>% 
  filter(TIC_DATE >= as.Date("2018-11-01")) %>%
  select(-LAST_DATE_PURCH, - DIFF_DAYS)

df_last_date_2_aziende <- df_aziende_attive2 %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_2_aziende)
boxplot(df_last_date_2_aziende$DIFF_DAYS)


df_clienti_attivi_2 <- merge(df_aziende_attive2, df_last_date_2_aziende, by="ID_CLI")

#notiamo cdal numero di righe dei due dataset he i clienti sono aumentati ma le transazioni ovvero il numero 
#di scontrini sono diminuite

##Calcolo Recency: l'ultimo acquisto dopo quanto tempo

quantili<- quantile(df_last_date_2_aziende$DIFF_DAYS, probs = c(0.25, 0.50, 0.75))
quantili
#25% 50% 75% 
#24  57 116

Recency_2_aziende = df_last_date_2_aziende %>% mutate(CLASS_R=case_when(DIFF_DAYS<quantili[1] ~ "Low",
                                                                        (DIFF_DAYS>=quantili[1]) & (DIFF_DAYS<quantili[3])~"Medium",
                                                                        (DIFF_DAYS>=quantili[3])~"High"))


Recency_2_aziende = mutate(Recency_2_aziende,CLASS_R=factor(CLASS_R,levels=c("Low","Medium","High")))


##Calcolo Frequency: ogni quanto acquisto

Frequency_2_aziende <- df_clienti_attivi_2 %>%
  filter(DIREZIONE==1) %>% group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>0)

boxplot(Frequency_2_aziende$N_SCONTRINI_PER_CLI)

quantili<- quantile(Frequency_2_aziende$N_SCONTRINI_PER_CLI,probs = c(0.50,0.70,0.90))
quantili
#50% 70% 90% 
#2   4   9 

Frequency_2_aziende <- Frequency_2_aziende %>% mutate(CLASS_F=case_when(N_SCONTRINI_PER_CLI<quantili[1] ~ "Low",
                                                                        (N_SCONTRINI_PER_CLI>=quantili[1]) & (N_SCONTRINI_PER_CLI<quantili[3])~"Medium",
                                                                        (N_SCONTRINI_PER_CLI>=quantili[3])~"High"))
Frequency_2_aziende = mutate(Frequency_2_aziende,CLASS_F=factor(CLASS_F,levels=c("Low","Medium","High")))

#Monetary Value (differenza tra importo lordo e sconto)

Monetary_Value_2_aziende = df_clienti_attivi_2 %>% filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(AMOUNT = sum(IMPORTO_LORDO) - sum(SCONTO))

quantili = quantile(Monetary_Value_2_aziende$AMOUNT,probs = c(0.25,0.50,0.75))
quantili
#   25%    50%    75% 
#  75.0025 224.6700 620.6275 


Monetary_Value_2_aziende = Monetary_Value_2_aziende %>% mutate(CLASS_M = case_when(AMOUNT<quantili[1]~"Low",
                                                                                   (AMOUNT>=quantili[1])&(AMOUNT<quantili[3])~"Medium",
                                                                                   AMOUNT>=quantili[3]~"High"))
Monetary_Value_2_aziende = mutate(Monetary_Value_2_aziende,CLASS_M=factor(CLASS_M,levels=c("Low","Medium","High")))

# Recency-Frequency

RF_aziende_2 <- merge(Recency_2_aziende, Frequency_2_aziende,by="ID_CLI")
RF_aziende_2 <- RF_aziende_2 %>% mutate(CLASS_RF_aziende_2 = case_when((CLASS_F=="Low")&(CLASS_R=="Low")~"One-Timer",
                                                                       (CLASS_F=="Low")&(CLASS_R=="Medium")~"One-Timer",
                                                                       (CLASS_F=="Low")&(CLASS_R=="High")~"Leaving",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="Low")~"Engaged",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="Medium")~"Engaged",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="High")~"Leaving",
                                                                       (CLASS_F=="High")&(CLASS_R=="Low")~"Top",
                                                                       (CLASS_F=="High")&(CLASS_R=="Medium")~"Top",
                                                                       (CLASS_F=="High")&(CLASS_R=="High")~"Leaving Top"))

Fedelta_2_aziende <- as.data.frame(with(RF_aziende_2,table(CLASS_RF_aziende_2)))
Fedelta_2_aziende <- Fedelta_2_aziende %>% mutate(CLASS_RF_aziende_2 = factor(CLASS_RF_aziende_2,levels = c("Leaving","One-Timer",
                                                                                                            "Engaged","Leaving Top",
                                                                                                            "Top")))

Fedelta_2_aziende # la maggior parte sono Engaged e leaving, continuano a esserci molti One_Timer

Fedelta_2_aziende$Freq <- (Fedelta_2_aziende$Freq / nrow(RF_aziende_2))

Fedelta_2_aziende_plot <- 
  ggplot(Fedelta_2_aziende,aes(CLASS_RF_aziende_2,Freq,fill=CLASS_RF_aziende_2)) + geom_bar(stat = "identity",width = 0.5)+
  ggtitle("Fedeltà")+ylab("Percentuale di Clienti")+ xlab("Fedeltà")+ scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Reds")+ 
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

Fedelta_2_aziende_plot


##### RFM

RFM_2_aziende <- RF_aziende_2 %>% select(-c("LAST_DATE_PURCH","CLASS_R","CLASS_F")) %>% 
  left_join(Monetary_Value_2_aziende,by="ID_CLI")
RFM_2_aziende <- mutate(RFM_2_aziende,CLASS_RF_aziende_2=factor(CLASS_RF_aziende_2))


RFM_2_aziende <- RFM_2_aziende %>% mutate(CLASSI_2 = case_when((CLASS_M=="Low") & (CLASS_RF_aziende_2=="One-Timer")~"Cheap",
                                                               (CLASS_M=="Low") & (CLASS_RF_aziende_2=="Leaving")~"Tin",
                                                               (CLASS_M=="Low") & (CLASS_RF_aziende_2=="Engaged")~"Copper",
                                                               (CLASS_M=="Low") & (CLASS_RF_aziende_2=="Leaving Top")~"Bronze",
                                                               (CLASS_M=="Low") & (CLASS_RF_aziende_2=="Top")~"Silver",
                                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_2=="One-Timer")~"Tin",
                                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_2=="Leaving")~"Copper",
                                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_2=="Engaged")~"Bronze",
                                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_2=="Leaving Top")~"Silver",
                                                               (CLASS_M=="Medium") & (CLASS_RF_aziende_2=="Top")~"Gold",
                                                               (CLASS_M=="High") & (CLASS_RF_aziende_2=="One-Timer")~"Copper",
                                                               (CLASS_M=="High") & (CLASS_RF_aziende_2=="Leaving")~"Bronze",
                                                               (CLASS_M=="High") & (CLASS_RF_aziende_2=="Engaged")~"Silver",
                                                               (CLASS_M=="High") & (CLASS_RF_aziende_2=="Leaving Top")~"Gold",
                                                               (CLASS_M=="High") & (CLASS_RF_aziende_2=="Top")~"Diamond"))

RFM_2_aziende = RFM_2_aziende %>% mutate(CLASSI_2 = factor(CLASSI_2,levels = c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")))
RFM_TOT2_aziende <- as.data.frame(with(RFM_2_aziende,table(CLASSI_2)))

RFM_TOT2_aziende#per la maggior parte sono bronze e Tin

RFM_TOT2_aziende$Freq <- (RFM_TOT2_aziende$Freq / nrow(RFM_2_aziende))


#EXPLORATORY ANALYSIS of RFM's dataframe
RFM_2_aziende_plot <- 
  ggplot(RFM_TOT2_aziende,aes(CLASSI_2,Freq,fill=CLASSI_2)) + geom_bar(stat = "identity")+
  labs(title = "Customer's distribution",size=18)+ylab("Percentuale clienti")+ scale_y_continuous(labels = percent) +
  scale_fill_manual(values=c("black","#2F4F4F","#801818","#CD7F32","#C0C0C0","gold","#B0E0E6"),guide=F)+
  theme_minimal()

RFM_2_aziende_plot

Old_class_az <- RFM_aziende_1 %>% mutate(VECCHIA_CLASSE = CLASSI_aziende_1) %>% select(ID_CLI, VECCHIA_CLASSE)

New_class_az <- RFM_2_aziende %>% mutate(NUOVA_CLASSE = CLASSI_2) %>% select(ID_CLI, NUOVA_CLASSE)

Confronto_RFM_az <- New_class_az %>% left_join(Old_class_az, by= "ID_CLI")
Confronto_RFM_az <- Confronto_RFM_az%>% mutate(VECCHIA_CLASSE = fct_explicit_na(VECCHIA_CLASSE,"Clienti_futuri"))
Confronto_RFM_az <- Confronto_RFM_az%>% mutate(NUOVA_CLASSE = fct_explicit_na(NUOVA_CLASSE,"Clienti_persi"))

matrice_classi_az=matrix(0,ncol = 7,nrow = 2)
colnames(matrice_classi_az) <- c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")
rownames(matrice_classi_az) <- c("Old_Clients","New_Clients")

Confronto_RFM_V_az <- Confronto_RFM_az %>% group_by(VECCHIA_CLASSE) %>% summarise(CLI_TOT_vecchi = n()) %>% mutate(CLASSE = VECCHIA_CLASSE) %>% select(- VECCHIA_CLASSE)

Confronto_RFM_N_az <- Confronto_RFM_az %>% group_by(NUOVA_CLASSE) %>% summarise(CLI_TOT_nuovi = n()) %>% mutate(CLASSE = NUOVA_CLASSE) %>% select(- NUOVA_CLASSE)

Confronto_RFM_TOT_az <- Confronto_RFM_V_az %>% right_join(Confronto_RFM_N_az, by="CLASSE")
Confronto_RFM_TOT_az <- Confronto_RFM_TOT_az %>% filter(CLASSE != "Clienti_persi")

matrice_classi_az[1,] <- c(Confronto_RFM_TOT_az$CLI_TOT_vecchi[1],Confronto_RFM_TOT_az$CLI_TOT_vecchi[2],Confronto_RFM_TOT_az$CLI_TOT_vecchi[3],Confronto_RFM_TOT_az$CLI_TOT_vecchi[4],Confronto_RFM_TOT_az$CLI_TOT_vecchi[5],Confronto_RFM_TOT_az$CLI_TOT_vecchi[6],Confronto_RFM_TOT_az$CLI_TOT_vecchi[7])
matrice_classi_az[2,] <- c(Confronto_RFM_TOT_az$CLI_TOT_nuovi[1],Confronto_RFM_TOT_az$CLI_TOT_nuovi[2],Confronto_RFM_TOT_az$CLI_TOT_nuovi[3],Confronto_RFM_TOT_az$CLI_TOT_nuovi[4],Confronto_RFM_TOT_az$CLI_TOT_nuovi[5],Confronto_RFM_TOT_az$CLI_TOT_nuovi[6],Confronto_RFM_TOT_az$CLI_TOT_nuovi[7])
df_matrice_classi_az <- as.data.frame(matrice_classi_az)


Confronto_RFM2_az <- Confronto_RFM_az %>% filter(VECCHIA_CLASSE != "Clienti_futuri" & NUOVA_CLASSE != "Clienti_persi")

Confronto_RFM3_az <- Confronto_RFM2_az %>% group_by(VECCHIA_CLASSE, NUOVA_CLASSE) %>% summarise(CONTO = n())


Nuovi_clientiRFm_az<- Confronto_RFM_az %>% filter(VECCHIA_CLASSE == "Clienti_futuri") %>% select(-VECCHIA_CLASSE)
Nuovi_clientiRFm2_az <- Nuovi_clientiRFm_az %>% group_by(NUOVA_CLASSE) %>% summarise(COUNT = n())
Nuovi_clientiRFm2_az <- Nuovi_clientiRFm2_az %>% mutate(PERC = percent(COUNT/sum(COUNT)))

#la percentuale dei nuovi clienti: meno del 50% fa parte della categoria Tin e Cheap, un 23% è Bronze e un 15% è Silver


Persi_clientiRFm_az<- Confronto_RFM_az %>% filter(NUOVA_CLASSE == "Clienti_persi") %>% select(-NUOVA_CLASSE)
Persi_clientiRFm2_az <- Persi_clientiRFm_az %>% group_by(VECCHIA_CLASSE) %>% summarise(COUNT = n())
Persi_clientiRFm2_az <- Persi_clientiRFm2_az %>% mutate(PERC = percent(COUNT/sum(COUNT)))

