#################

# MODELLO RFM per la categoria persone

################

lastdate = df_7_persone %>% summarise(max(TIC_DATE))
lastdate=lastdate[1,1]
## prima data di acquisto risale al 2018-05-01, l'ultima data di acquisto risale al 2019-04-30

#vogliamo trovare i clienti attivi (80%)

#troviamo l'ultimo giorno di acquisto per ogni cliente
#e quanti giorni sono passati dall'ultimo acquisto in generale

df_last_date <- df_7_persone %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date)
boxplot(df_last_date$DIFF_DAYS)

giorni_max=quantile(df_last_date$DIFF_DAYS, probs = c(0.80, 0.85, 0.90))
giorni_max

#decidiamo di considerare attivi l'ottanta percento dei clienti

giorni_max = giorni_max[1]

clienti_attivi <- df_last_date %>% filter(DIFF_DAYS<=giorni_max)

df_clienti_attivi <- merge( df_7_persone, clienti_attivi, by="ID_CLI")

################### MODELLO RFM DAL 2018/08/01 AL 2019/01/31 #######################

df_clienti_attivi1 <- df_clienti_attivi %>% 
  filter(TIC_DATE <= as.Date("2019-01-31") & TIC_DATE >= as.Date("2018-08-01")) %>%
  select(-LAST_DATE_PURCH, - DIFF_DAYS)

df_last_date_persone_1 <- df_clienti_attivi1 %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_persone_1)
boxplot(df_last_date_persone_1$DIFF_DAYS)


df_clienti_attivi_1 <- merge(df_clienti_attivi1, df_last_date_persone_1, by="ID_CLI")

##Calcolo Recency: l'ultimo acquisto dopo quanto tempo

quantili<- quantile(df_last_date_persone_1$DIFF_DAYS, probs = c(0.25, 0.50, 0.75))
quantili
#25% 50% 75% 
#109 143 181 

Recency_persone_1 = df_last_date_persone_1 %>% mutate(CLASS_R=case_when(DIFF_DAYS<quantili[1] ~ "Low",
                                                                        (DIFF_DAYS>=quantili[1]) & (DIFF_DAYS<quantili[3])~"Medium",
                                                                        (DIFF_DAYS>=quantili[3])~"High"))


Recency_persone_1 = mutate(Recency_persone_1,CLASS_R=factor(CLASS_R,levels=c("Low","Medium","High")))


##Calcolo Frequency: ogni quanto acquisto

Frequency_persone_1 <- df_clienti_attivi_1 %>%
  filter(DIREZIONE==1) %>% group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>0)

boxplot(Frequency_persone_1$N_SCONTRINI_PER_CLI)

quantili<- quantile(Frequency_persone_1$N_SCONTRINI_PER_CLI,probs = c(0.50,0.70,0.90))
quantili
#50% 70% 90% 
#2   4   8 

Frequency_persone_1 <- Frequency_persone_1 %>% mutate(CLASS_F=case_when(N_SCONTRINI_PER_CLI< quantili[1] ~ "Low",
                                                                        (N_SCONTRINI_PER_CLI>=quantili[1]) & (N_SCONTRINI_PER_CLI<quantili[3])~"Medium",
                                                                        (N_SCONTRINI_PER_CLI>=quantili[3])~"High"))
Frequency_persone_1 = mutate(Frequency_persone_1,CLASS_F=factor(CLASS_F,levels=c("Low","Medium","High")))

#Monetary Value (differenza tra importo lordo e sconto)

Monetary_Value_persone_1 = df_clienti_attivi_1 %>% filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(AMOUNT = sum(IMPORTO_LORDO) - sum(SCONTO))

quantili = quantile(Monetary_Value_persone_1$AMOUNT,probs = c(0.25,0.50,0.75))
quantili
#     25%      50%      75% 
#56.5900 163.8900 464.8075 

Monetary_Value_persone_1 = Monetary_Value_persone_1 %>% mutate(CLASS_M = case_when(AMOUNT<quantili[1]~"Low",
                                                                                   (AMOUNT>=quantili[1])&(AMOUNT<quantili[3])~"Medium",
                                                                                   AMOUNT>=quantili[3]~"High"))
Monetary_Value_persone_1 = mutate(Monetary_Value_persone_1,CLASS_M=factor(CLASS_M,levels=c("Low","Medium","High")))

# Recency-Frequency

RF_persone_1 <- merge(Recency_persone_1, Frequency_persone_1,by="ID_CLI")
RF_persone_1 <- RF_persone_1 %>% mutate(CLASS_RF_persone_1 = case_when((CLASS_F=="Low")&(CLASS_R=="Low")~"One-Timer",
                                                                       (CLASS_F=="Low")&(CLASS_R=="Medium")~"One-Timer",
                                                                       (CLASS_F=="Low")&(CLASS_R=="High")~"Leaving",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="Low")~"Engaged",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="Medium")~"Engaged",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="High")~"Leaving",
                                                                       (CLASS_F=="High")&(CLASS_R=="Low")~"Top",
                                                                       (CLASS_F=="High")&(CLASS_R=="Medium")~"Top",
                                                                       (CLASS_F=="High")&(CLASS_R=="High")~"Leaving Top"))

Fedelta_persone_1 <- as.data.frame(with(RF_persone_1,table(CLASS_RF_persone_1)))
Fedelta_persone_1 <- Fedelta_persone_1 %>% mutate(CLASS_RF_persone_1 = factor(CLASS_RF_persone_1,levels = c("Leaving","One-Timer",
                                                                                                            "Engaged","Leaving Top",
                                                                                                            "Top")))
Fedelta_persone_1
summary(Fedelta_persone_1)

#per la maggior parte sono Engaged e Leaving

Fedelta_persone_1$Freq <- (Fedelta_persone_1$Freq / nrow(RF_persone_1))

Fedelta_persone_1_plot <- 
  ggplot(Fedelta_persone_1,aes(CLASS_RF_persone_1,Freq,fill=CLASS_RF_persone_1)) + geom_bar(stat = "identity",width = 0.5)+
  ggtitle("Fedeltà")+ylab("Percentuale di Clienti")+xlab("Fedeltà")+
  scale_fill_brewer(palette = "Reds")+ scale_y_continuous(labels = percent) +
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

Fedelta_persone_1_plot

##### RFM_persone_1

RFM_persone_1 <- RF_persone_1 %>% select(-c("LAST_DATE_PURCH","CLASS_R","CLASS_F")) %>% 
  left_join(Monetary_Value_persone_1,by="ID_CLI")
RFM_persone_1 <- mutate(RFM_persone_1,CLASS_RF_persone_1=factor(CLASS_RF_persone_1))


RFM_persone_1 <- RFM_persone_1 %>% mutate(CLASSI_persone_1 = case_when((CLASS_M=="Low") & (CLASS_RF_persone_1=="One-Timer")~"Cheap",
                                                                       (CLASS_M=="Low") & (CLASS_RF_persone_1=="Leaving")~"Tin",
                                                                       (CLASS_M=="Low") & (CLASS_RF_persone_1=="Engaged")~"Copper",
                                                                       (CLASS_M=="Low") & (CLASS_RF_persone_1=="Leaving Top")~"Bronze",
                                                                       (CLASS_M=="Low") & (CLASS_RF_persone_1=="Top")~"Silver",
                                                                       (CLASS_M=="Medium") & (CLASS_RF_persone_1=="One-Timer")~"Tin",
                                                                       (CLASS_M=="Medium") & (CLASS_RF_persone_1=="Leaving")~"Copper",
                                                                       (CLASS_M=="Medium") & (CLASS_RF_persone_1=="Engaged")~"Bronze",
                                                                       (CLASS_M=="Medium") & (CLASS_RF_persone_1=="Leaving Top")~"Silver",
                                                                       (CLASS_M=="Medium") & (CLASS_RF_persone_1=="Top")~"Gold",
                                                                       (CLASS_M=="High") & (CLASS_RF_persone_1=="One-Timer")~"Copper",
                                                                       (CLASS_M=="High") & (CLASS_RF_persone_1=="Leaving")~"Bronze",
                                                                       (CLASS_M=="High") & (CLASS_RF_persone_1=="Engaged")~"Silver",
                                                                       (CLASS_M=="High") & (CLASS_RF_persone_1=="Leaving Top")~"Gold",
                                                                       (CLASS_M=="High") & (CLASS_RF_persone_1=="Top")~"Diamond"))
RFM_persone_1 = RFM_persone_1 %>% mutate(CLASSI_persone_1 = factor(CLASSI_persone_1,levels = c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")))
RFM_TOT_persone_1 <- as.data.frame(with(RFM_persone_1,table(CLASSI_persone_1)))

RFM_TOT_persone_1 #la maggior parte dei clienti appartengono alla categoria Bronze, però troppi
#clienti appartengono alla categoria tin.

RFM_TOT_persone_1$Freq <- (RFM_TOT_persone_1$Freq / nrow(RFM_persone_1))


#EXPLORATORY ANALYSIS of RFM's dataframe
RFM_persone_1_plot <- 
  ggplot(RFM_TOT_persone_1,aes(CLASSI_persone_1,Freq,fill=CLASSI_persone_1)) + geom_bar(stat = "identity")+
  labs(title = "Customer's distribution",size=18)+ylab("Percentuale clienti")+ scale_y_continuous(labels = percent)+
  scale_fill_manual(values=c("black","#2F4F4F","#801818","#CD7F32","#C0C0C0","gold","#B0E0E6"),guide=F)+
  theme_minimal()

RFM_persone_1_plot


################ MODELLO RFM DAL 2018/11/01 AL 2019/04/30 ########################

df_clienti_attivi2 <- df_clienti_attivi %>% 
  filter(TIC_DATE >= as.Date("2018-11-01")) %>%
  select(-LAST_DATE_PURCH, - DIFF_DAYS)

df_last_date_2_persone <- df_clienti_attivi2 %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_2_persone)
boxplot(df_last_date_2_persone$DIFF_DAYS)


df_clienti_attivi_2 <- merge(df_clienti_attivi2, df_last_date_2_persone, by="ID_CLI")

#notiamo cdal numero di righe dei due dataset he i clienti sono aumentati ma le transazioni ovvero il numero 
#di scontrini sono diminuite

##Calcolo Recency: l'ultimo acquisto dopo quanto tempo

quantili<- quantile(df_last_date_2_persone$DIFF_DAYS, probs = c(0.25, 0.50, 0.75))
quantili
#25% 50% 75% 
#24  58 114 

Recency_2_persone = df_last_date_2_persone %>% mutate(CLASS_R=case_when(DIFF_DAYS<quantili[1] ~ "Low",
                                                                        (DIFF_DAYS>=quantili[1]) & (DIFF_DAYS<quantili[3])~"Medium",
                                                                        (DIFF_DAYS>=quantili[3])~"High"))


Recency_2_persone = mutate(Recency_2_persone,CLASS_R=factor(CLASS_R,levels=c("Low","Medium","High")))


##Calcolo Frequency: ogni quanto acquisto

Frequency_2_persone <- df_clienti_attivi_2 %>%
  filter(DIREZIONE==1) %>% group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>0)

boxplot(Frequency_2_persone$N_SCONTRINI_PER_CLI)

quantili<- quantile(Frequency_2_persone$N_SCONTRINI_PER_CLI,probs = c(0.50,0.70,0.90))
quantili
#50% 70% 90% 
#2   3   7 


Frequency_2_persone <- Frequency_2_persone %>% mutate(CLASS_F=case_when(N_SCONTRINI_PER_CLI<quantili[1] ~ "Low",
                                                                        (N_SCONTRINI_PER_CLI>=quantili[1]) & (N_SCONTRINI_PER_CLI<quantili[3])~"Medium",
                                                                        (N_SCONTRINI_PER_CLI>=quantili[3])~"High"))
Frequency_2_persone = mutate(Frequency_2_persone,CLASS_F=factor(CLASS_F,levels=c("Low","Medium","High")))

#Monetary Value (differenza tra importo lordo e sconto)

Monetary_Value_2_persone = df_clienti_attivi_2 %>% filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(AMOUNT = sum(IMPORTO_LORDO) - sum(SCONTO))

quantili = quantile(Monetary_Value_2_persone$AMOUNT,probs = c(0.25,0.50,0.75))
quantili
#25%      50%      75% 
#48.4500 138.5500 388.9975 


Monetary_Value_2_persone = Monetary_Value_2_persone %>% mutate(CLASS_M = case_when(AMOUNT<quantili[1]~"Low",
                                                                                   (AMOUNT>=quantili[1])&(AMOUNT<quantili[3])~"Medium",
                                                                                   AMOUNT>=quantili[3]~"High"))
Monetary_Value_2_persone = mutate(Monetary_Value_2_persone,CLASS_M=factor(CLASS_M,levels=c("Low","Medium","High")))

# Recency-Frequency

RF_persone_2 <- merge(Recency_2_persone, Frequency_2_persone,by="ID_CLI")
RF_persone_2 <- RF_persone_2 %>% mutate(CLASS_RF_persone_2 = case_when((CLASS_F=="Low")&(CLASS_R=="Low")~"One-Timer",
                                                                       (CLASS_F=="Low")&(CLASS_R=="Medium")~"One-Timer",
                                                                       (CLASS_F=="Low")&(CLASS_R=="High")~"Leaving",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="Low")~"Engaged",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="Medium")~"Engaged",
                                                                       (CLASS_F=="Medium")&(CLASS_R=="High")~"Leaving",
                                                                       (CLASS_F=="High")&(CLASS_R=="Low")~"Top",
                                                                       (CLASS_F=="High")&(CLASS_R=="Medium")~"Top",
                                                                       (CLASS_F=="High")&(CLASS_R=="High")~"Leaving Top"))

Fedelta_2_persone <- as.data.frame(with(RF_persone_2,table(CLASS_RF_persone_2)))
Fedelta_2_persone <- Fedelta_2_persone %>% mutate(CLASS_RF_persone_2 = factor(CLASS_RF_persone_2,levels = c("Leaving","One-Timer",
                                                                                                            "Engaged","Leaving Top",
                                                                                                            "Top")))

Fedelta_2_persone # la maggior parte sono Engaged, continuano a esserci molti One_Timer

Fedelta_2_persone$Freq <- (Fedelta_2_persone$Freq / nrow(RF_persone_2))

Fedelta_2_persone_plot <- 
  ggplot(Fedelta_2_persone,aes(CLASS_RF_persone_2,Freq,fill=CLASS_RF_persone_2)) + geom_bar(stat = "identity",width = 0.5)+
  ggtitle("Fedeltà")+ylab("Percentuale di Clienti")+ xlab("Fedeltà")+ scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Reds")+ 
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

Fedelta_2_persone_plot


##### RFM

RFM_2_persone <- RF_persone_2 %>% select(-c("LAST_DATE_PURCH","CLASS_R","CLASS_F")) %>% 
  left_join(Monetary_Value_2_persone,by="ID_CLI")
RFM_2_persone <- mutate(RFM_2_persone,CLASS_RF_persone_2=factor(CLASS_RF_persone_2))


RFM_2_persone <- RFM_2_persone %>% mutate(CLASSI_2 = case_when((CLASS_M=="Low") & (CLASS_RF_persone_2=="One-Timer")~"Cheap",
                                                               (CLASS_M=="Low") & (CLASS_RF_persone_2=="Leaving")~"Tin",
                                                               (CLASS_M=="Low") & (CLASS_RF_persone_2=="Engaged")~"Copper",
                                                               (CLASS_M=="Low") & (CLASS_RF_persone_2=="Leaving Top")~"Bronze",
                                                               (CLASS_M=="Low") & (CLASS_RF_persone_2=="Top")~"Silver",
                                                               (CLASS_M=="Medium") & (CLASS_RF_persone_2=="One-Timer")~"Tin",
                                                               (CLASS_M=="Medium") & (CLASS_RF_persone_2=="Leaving")~"Copper",
                                                               (CLASS_M=="Medium") & (CLASS_RF_persone_2=="Engaged")~"Bronze",
                                                               (CLASS_M=="Medium") & (CLASS_RF_persone_2=="Leaving Top")~"Silver",
                                                               (CLASS_M=="Medium") & (CLASS_RF_persone_2=="Top")~"Gold",
                                                               (CLASS_M=="High") & (CLASS_RF_persone_2=="One-Timer")~"Copper",
                                                               (CLASS_M=="High") & (CLASS_RF_persone_2=="Leaving")~"Bronze",
                                                               (CLASS_M=="High") & (CLASS_RF_persone_2=="Engaged")~"Silver",
                                                               (CLASS_M=="High") & (CLASS_RF_persone_2=="Leaving Top")~"Gold",
                                                               (CLASS_M=="High") & (CLASS_RF_persone_2=="Top")~"Diamond"))

RFM_2_persone = RFM_2_persone %>% mutate(CLASSI_2 = factor(CLASSI_2,levels = c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")))
RFM_TOT2_persone <- as.data.frame(with(RFM_2_persone,table(CLASSI_2)))

RFM_TOT2_persone#per la maggior parte sono bronze e Tin e copper

RFM_TOT2_persone$Freq <- (RFM_TOT2_persone$Freq / nrow(RFM_2_persone))


#EXPLORATORY ANALYSIS of RFM's dataframe
RFM_2_persone_plot <- 
  ggplot(RFM_TOT2_persone,aes(CLASSI_2,Freq,fill=CLASSI_2)) + geom_bar(stat = "identity")+
  labs(title = "Customer's distribution",size=18)+ylab("Percentuale clienti")+ scale_y_continuous(labels = percent) +
  scale_fill_manual(values=c("black","#2F4F4F","#801818","#CD7F32","#C0C0C0","gold","#B0E0E6"),guide=F)+
  theme_minimal()

RFM_2_persone_plot

Old_class <- RFM_persone_1 %>% mutate(VECCHIA_CLASSE = CLASSI_persone_1) %>% select(ID_CLI, VECCHIA_CLASSE)

New_class <- RFM_2_persone %>% mutate(NUOVA_CLASSE = CLASSI_2) %>% select(ID_CLI, NUOVA_CLASSE)

Confronto_RFM <- New_class %>% left_join(Old_class, by= "ID_CLI")
Confronto_RFM <- Confronto_RFM%>% mutate(VECCHIA_CLASSE = fct_explicit_na(VECCHIA_CLASSE,"Clienti_futuri"))

Nuovi_clientiRFm <- Confronto_RFM %>% filter(VECCHIA_CLASSE == "Clienti_futuri") %>% select(-VECCHIA_CLASSE)
Nuovi_clientiRFm2 <- Nuovi_clientiRFm %>% group_by(NUOVA_CLASSE) %>% summarise(COUNT = n())
Nuovi_clientiRFm2 <- Nuovi_clientiRFm2 %>% mutate(PERC = percent(COUNT/sum(COUNT)))

#la percentuale dei nuovi clienti: più del 50% fa parte della categoria Tin e Cheap

matrice_classi=matrix(0,ncol = 7,nrow = 2)
colnames(matrice_classi) <- c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")
rownames(matrice_classi) <- c("Old_Clients","New_Clients")

Confronto_RFM_V <- Confronto_RFM %>% group_by(VECCHIA_CLASSE) %>% summarise(CLI_TOT_vecchi = n()) %>% mutate(CLASSE = VECCHIA_CLASSE) %>% select(- VECCHIA_CLASSE)

Confronto_RFM_N <- Confronto_RFM %>% group_by(NUOVA_CLASSE) %>% summarise(CLI_TOT_nuovi = n()) %>% mutate(CLASSE = NUOVA_CLASSE) %>% select(- NUOVA_CLASSE)

Confronto_RFM_TOT <- Confronto_RFM_V %>% right_join(Confronto_RFM_N, by="CLASSE")

matrice_classi[1,] <- c(Confronto_RFM_TOT$CLI_TOT_vecchi[1],Confronto_RFM_TOT$CLI_TOT_vecchi[2],Confronto_RFM_TOT$CLI_TOT_vecchi[3],Confronto_RFM_TOT$CLI_TOT_vecchi[4],Confronto_RFM_TOT$CLI_TOT_vecchi[5],Confronto_RFM_TOT$CLI_TOT_vecchi[6],Confronto_RFM_TOT$CLI_TOT_vecchi[7])
matrice_classi[2,] <- c(Confronto_RFM_TOT$CLI_TOT_nuovi[1],Confronto_RFM_TOT$CLI_TOT_nuovi[2],Confronto_RFM_TOT$CLI_TOT_nuovi[3],Confronto_RFM_TOT$CLI_TOT_nuovi[4],Confronto_RFM_TOT$CLI_TOT_nuovi[5],Confronto_RFM_TOT$CLI_TOT_nuovi[6],Confronto_RFM_TOT$CLI_TOT_nuovi[7])
df_matrice_classi <- as.data.frame(matrice_classi)


Confronto_RFM2 <- Confronto_RFM %>% filter(VECCHIA_CLASSE != "Clienti_futuri")

Confronto_RFM3 <- Confronto_RFM2 %>% group_by(VECCHIA_CLASSE, NUOVA_CLASSE) %>% summarise(CONTO = n())




