#################

# MODELLO RFM per la categoria aziende

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

################### MODELLO RFM DAL 2018/09/01 AL 2019/01/31 #######################

df_clienti_attivi1 <- df_clienti_attivi %>% 
  filter(TIC_DATE <= as.Date("2019-01-31") & TIC_DATE >= as.Date("2018-09-01")) %>%
  select(-LAST_DATE_PURCH, - DIFF_DAYS)

df_last_date_1 <- df_clienti_attivi1 %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_1)
boxplot(df_last_date_1$DIFF_DAYS)


df_clienti_attivi_1 <- merge(df_clienti_attivi1, df_last_date_1, by="ID_CLI")

##Calcolo Recency: l'ultimo acquisto dopo quanto tempo?

quantili<- quantile(df_last_date_1$DIFF_DAYS, probs = c(0.25, 0.50, 0.75))
quantili
#25% 50% 75% 
#109 142 179 

recency_1 = df_last_date_1 %>% mutate(CLASS_R=case_when(DIFF_DAYS<quantili[1] ~ "Low",
                                                        (DIFF_DAYS>=quantili[1]) & (DIFF_DAYS<quantili[3])~"Medium",
                                                        (DIFF_DAYS>=quantili[3])~"High"))


recency_1 = mutate(recency_1,CLASS_R=factor(CLASS_R,levels=c("Low","Medium","High")))


##Calcolo Frequency: ogni quanto acquisto?

Frequency_1 <- df_clienti_attivi_1 %>%
  filter(DIREZIONE==1) %>% group_by(ID_CLI) %>%
  summarise(N_TRANSIZIONI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_TRANSIZIONI_PER_CLI>0)

boxplot(Frequency_1$N_TRANSIZIONI_PER_CLI)

quantili<- quantile(Frequency_1$N_TRANSIZIONI_PER_CLI,probs = c(0.50,0.70,0.90))
quantili

Frequency_1 <- Frequency_1 %>% mutate(CLASS_F=case_when(N_TRANSIZIONI_PER_CLI<quantili[1] ~ "Low",
                                                        (N_TRANSIZIONI_PER_CLI>=quantili[1]) & (N_TRANSIZIONI_PER_CLI<quantili[3])~"Medium",
                                                        (N_TRANSIZIONI_PER_CLI>quantili[3])~"High"))
Frequency_1 = mutate(Frequency_1,CLASS_F=factor(CLASS_F,levels=c("Low","Medium","High")))

#Monetary Value (differenza tra importo lordo e sconto)

Monetary_Value_1 = df_clienti_attivi_1 %>% filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(AMOUNT = sum(IMPORTO_LORDO) - sum(SCONTO))

quantili = quantile(Monetary_Value_1$AMOUNT,probs = c(0.25,0.50,0.75))
quantili

Monetary_Value_1 = Monetary_Value_1 %>% mutate(CLASS_M = case_when(AMOUNT<quantili[1]~"Low",
                                                                   (AMOUNT>=quantili[1])&(AMOUNT<quantili[3])~"Medium",
                                                                   AMOUNT>quantili[3]~"High"))
Monetary_Value_1 = mutate(Monetary_Value_1,CLASS_M=factor(CLASS_M,levels=c("Low","Medium","High")))

# Recency-Frequency

RF_1 <- merge(recency_1, Frequency_1,by="ID_CLI")
RF_1 <- RF_1 %>% mutate(CLASS_RF_1 = case_when((CLASS_F=="Low")&(CLASS_R=="Low")~"One-Timer",
                                               (CLASS_F=="Low")&(CLASS_R=="Medium")~"One-Timer",
                                               (CLASS_F=="Low")&(CLASS_R=="High")~"Leaving",
                                               (CLASS_F=="Medium")&(CLASS_R=="Low")~"Engaged",
                                               (CLASS_F=="Medium")&(CLASS_R=="Medium")~"Engaged",
                                               (CLASS_F=="Medium")&(CLASS_R=="High")~"Leaving",
                                               (CLASS_F=="High")&(CLASS_R=="Low")~"Top",
                                               (CLASS_F=="High")&(CLASS_R=="Medium")~"Top",
                                               (CLASS_F=="High")&(CLASS_R=="High")~"Leaving Top"))

Fedelta_1 <- as.data.frame(with(RF_1,table(CLASS_RF_1)))
Fedelta_1 <- Fedelta_1 %>% mutate(CLASS_RF_1 = factor(CLASS_RF_1,levels = c("Leaving","One-Timer",
                                                                            "Engaged","Leaving Top",
                                                                            "Top")))
Fedelta_1
summary(Fedelta_1)

#non ci sono Leaving e Leaving Top, per la maggior parte sono Engaged e One-Timer

Fedelta_1$Freq <- (Fedelta_1$Freq / nrow(RF_1))

Fedelta_1_plot <- 
  ggplot(Fedelta_1,aes(CLASS_RF_1,Freq,fill=CLASS_RF_1)) + geom_bar(stat = "identity",width = 0.5)+
  ggtitle("Fedeltà")+ylab("Percentuale di Clienti")+xlab("Fedeltà")+
  scale_fill_brewer(palette = "Reds")+ scale_y_continuous(labels = percent) +
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

Fedelta_1_plot

################### rendere più carino !!!!!!!!!!!!!!

##### RFM_1

RFM_1 <- RF_1 %>% select(-c("LAST_DATE_PURCH","CLASS_R","CLASS_F")) %>% 
  left_join(Monetary_Value_1,by="ID_CLI")
RFM_1 <- mutate(RFM_1,CLASS_RF_1=factor(CLASS_RF_1))


RFM_1 <- RFM_1 %>% mutate(CLASSI_1 = case_when((CLASS_M=="Low") & (CLASS_RF_1=="One-Timer")~"Cheap",
                                               (CLASS_M=="Low") & (CLASS_RF_1=="Leaving")~"Tin",
                                               (CLASS_M=="Low") & (CLASS_RF_1=="Engaged")~"Copper",
                                               (CLASS_M=="Low") & (CLASS_RF_1=="Leaving Top")~"Bronze",
                                               (CLASS_M=="Low") & (CLASS_RF_1=="Top")~"Silver",
                                               (CLASS_M=="Medium") & (CLASS_RF_1=="One-Timer")~"Tin",
                                               (CLASS_M=="Medium") & (CLASS_RF_1=="Leaving")~"Copper",
                                               (CLASS_M=="Medium") & (CLASS_RF_1=="Engaged")~"Bronze",
                                               (CLASS_M=="Medium") & (CLASS_RF_1=="Leaving Top")~"Silver",
                                               (CLASS_M=="Medium") & (CLASS_RF_1=="Top")~"Gold",
                                               (CLASS_M=="High") & (CLASS_RF_1=="One-Timer")~"Copper",
                                               (CLASS_M=="High") & (CLASS_RF_1=="Leaving")~"Bronze",
                                               (CLASS_M=="High") & (CLASS_RF_1=="Engaged")~"Silver",
                                               (CLASS_M=="High") & (CLASS_RF_1=="Leaving Top")~"Gold",
                                               (CLASS_M=="High") & (CLASS_RF_1=="Top")~"Diamond"))
RFM_1 = RFM_1 %>% mutate(CLASSI_1 = factor(CLASSI_1,levels = c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")))
RFM_TOT1 <- as.data.frame(with(RFM_1,table(CLASSI_1)))

RFM_TOT1 #la maggior parte dei clienti appartengono alla categoria Bronze, però troppi
#clienti appartengono alla categoria cheap e tin.

RFM_TOT1$Freq <- (RFM_TOT1$Freq / nrow(RFM_1))


#EXPLORATORY ANALYSIS of RFM's dataframe
RFM_1_plot <- 
  ggplot(RFM_TOT1,aes(CLASSI_1,Freq,fill=CLASSI_1)) + geom_bar(stat = "identity")+
  labs(title = "Customer's distribution",size=18)+ylab("Percentuale clienti")+ scale_y_continuous(labels = percent)+
  scale_fill_manual(values=c("black","#2F4F4F","#801818","#CD7F32","#C0C0C0","gold","#B0E0E6"),guide=F)+
  theme_minimal()

RFM_1_plot


################ MODELLO RFM DAL 2018/12/01 AL 2019/04/30 ########################

df_clienti_attivi2 <- df_clienti_attivi %>% 
  filter(TIC_DATE >= as.Date("2018-12-01")) %>%
  select(-LAST_DATE_PURCH, - DIFF_DAYS)

df_last_date_2 <- df_clienti_attivi2 %>%
  filter(DIREZIONE == 1)%>%
  group_by(ID_CLI) %>%
  summarise(LAST_DATE_PURCH= max(TIC_DATE)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(lastdate,LAST_DATE_PURCH,units = "days")))


summary(df_last_date_2)
boxplot(df_last_date_2$DIFF_DAYS)


df_clienti_attivi_2 <- merge(df_clienti_attivi2, df_last_date_2, by="ID_CLI")

#notiamo cdal numero di righe dei due dataset he i clienti sono aumentati ma le transazioni ovvero il numero 
#di scontrini sono diminuite-

##Calcolo Recency: l'ultimo acquisto dopo quanto tempo?

quantili<- quantile(df_last_date_2$DIFF_DAYS, probs = c(0.25, 0.50, 0.75))
quantili
#25% 50% 75% 
#22  48  92 

recency_2 = df_last_date_2 %>% mutate(CLASS_R=case_when(DIFF_DAYS<quantili[1] ~ "Low",
                                                        (DIFF_DAYS>=quantili[1]) & (DIFF_DAYS<quantili[3])~"Medium",
                                                        (DIFF_DAYS>=quantili[3])~"High"))


recency_2 = mutate(recency_2,CLASS_R=factor(CLASS_R,levels=c("Low","Medium","High")))


##Calcolo Frequency: ogni quanto acquisto?

Frequency_2 <- df_clienti_attivi_2 %>%
  filter(DIREZIONE==1) %>% group_by(ID_CLI) %>%
  summarise(N_ACQUISTI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_ACQUISTI_PER_CLI>0)

boxplot(Frequency_2$N_ACQUISTI_PER_CLI)

quantili<- quantile(Frequency_2$N_ACQUISTI_PER_CLI,probs = c(0.50,0.70,0.90))
quantili

Frequency_2 <- Frequency_2 %>% mutate(CLASS_F=case_when(N_ACQUISTI_PER_CLI<quantili[1] ~ "Low",
                                                        (N_ACQUISTI_PER_CLI>=quantili[1]) & (N_ACQUISTI_PER_CLI<quantili[3])~"Medium",
                                                        (N_ACQUISTI_PER_CLI>quantili[3])~"High"))
Frequency_2 = mutate(Frequency_2,CLASS_F=factor(CLASS_F,levels=c("Low","Medium","High")))

#Monetary Value (differenza tra importo lordo e sconto)

Monetary_Value_2 = df_clienti_attivi_2 %>% filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(AMOUNT = sum(IMPORTO_LORDO) - sum(SCONTO))

quantili = quantile(Monetary_Value_2$AMOUNT,probs = c(0.25,0.50,0.75))
quantili
#


Monetary_Value_2 = Monetary_Value_2 %>% mutate(CLASS_M = case_when(AMOUNT<quantili[1]~"Low",
                                                                   (AMOUNT>=quantili[1])&(AMOUNT<quantili[3])~"Medium",
                                                                   AMOUNT>quantili[3]~"High"))
Monetary_Value_2 = mutate(Monetary_Value_2,CLASS_M=factor(CLASS_M,levels=c("Low","Medium","High")))

# Recency-Frequency

RF_2 <- merge(recency_2, Frequency_2,by="ID_CLI")
RF_2 <- RF %>% mutate(CLASS_RF_2 = case_when((CLASS_F=="Low")&(CLASS_R=="Low")~"One-Timer",
                                             (CLASS_F=="Low")&(CLASS_R=="Medium")~"One-Timer",
                                             (CLASS_F=="Low")&(CLASS_R=="High")~"Leaving",
                                             (CLASS_F=="Medium")&(CLASS_R=="Low")~"Engaged",
                                             (CLASS_F=="Medium")&(CLASS_R=="Medium")~"Engaged",
                                             (CLASS_F=="Medium")&(CLASS_R=="High")~"Leaving",
                                             (CLASS_F=="High")&(CLASS_R=="Low")~"Top",
                                             (CLASS_F=="High")&(CLASS_R=="Medium")~"Top",
                                             (CLASS_F=="High")&(CLASS_R=="High")~"Leaving Top"))

Fedelta_2 <- as.data.frame(with(RF_2,table(CLASS_RF_2)))
Fedelta_2 <- Fedelta_2 %>% mutate(CLASS_RF_2 = factor(CLASS_RF_2,levels = c("Leaving","One-Timer",
                                                                            "Engaged","Leaving Top",
                                                                            "Top")))

Fedelta_2 #ci sono Leaving e Leaving Top, la maggior parte sono Engaged, continuano a esserci molti One_Timer

Fedelta_2$Freq <- (Fedelta_2$Freq / nrow(RF_2))

Fedelta_2_plot <- 
  ggplot(Fedelta_2,aes(CLASS_RF_2,Freq,fill=CLASS_RF_2)) + geom_bar(stat = "identity",width = 0.5)+
  ggtitle("Fedeltà")+ylab("Percentuale di Clienti")+ xlab("Fedeltà")+ scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Reds")+ 
  theme_bw()+theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

Fedelta_2_plot

################### rendere più carino e mettere in percentuale

##### RFM

RFM_2 <- RF_2 %>% select(-c("LAST_DATE_PURCH","CLASS_R","CLASS_F")) %>% 
  left_join(Monetary_Value_2,by="ID_CLI")
RFM_2 <- mutate(RFM_2,CLASS_RF_2=factor(CLASS_RF_2))


RFM_2 <- RFM_2 %>% mutate(CLASSI_2 = case_when((CLASS_M=="Low") & (CLASS_RF_2=="One-Timer")~"Cheap",
                                               (CLASS_M=="Low") & (CLASS_RF_2=="Leaving")~"Tin",
                                               (CLASS_M=="Low") & (CLASS_RF_2=="Engaged")~"Copper",
                                               (CLASS_M=="Low") & (CLASS_RF_2=="Leaving Top")~"Bronze",
                                               (CLASS_M=="Low") & (CLASS_RF_2=="Top")~"Silver",
                                               (CLASS_M=="Medium") & (CLASS_RF_2=="One-Timer")~"Tin",
                                               (CLASS_M=="Medium") & (CLASS_RF_2=="Leaving")~"Copper",
                                               (CLASS_M=="Medium") & (CLASS_RF_2=="Engaged")~"Bronze",
                                               (CLASS_M=="Medium") & (CLASS_RF_2=="Leaving Top")~"Silver",
                                               (CLASS_M=="Medium") & (CLASS_RF_2=="Top")~"Gold",
                                               (CLASS_M=="High") & (CLASS_RF_2=="One-Timer")~"Copper",
                                               (CLASS_M=="High") & (CLASS_RF_2=="Leaving")~"Bronze",
                                               (CLASS_M=="High") & (CLASS_RF_2=="Engaged")~"Silver",
                                               (CLASS_M=="High") & (CLASS_RF_2=="Leaving Top")~"Gold",
                                               (CLASS_M=="High") & (CLASS_RF_2=="Top")~"Diamond"))

RFM_2 = RFM_2 %>% mutate(CLASSI_2 = factor(CLASSI_2,levels = c("Cheap","Tin","Copper","Bronze","Silver","Gold","Diamond")))
RFM_TOT2 <- as.data.frame(with(RFM_2,table(CLASSI_2)))

RFM_TOT2#per la maggior parte sono bronze e Tin

RFM_TOT2$Freq <- (RFM_TOT2$Freq / nrow(RFM_2))


#EXPLORATORY ANALYSIS of RFM's dataframe
RFM_2_plot <- 
  ggplot(RFM_TOT2,aes(CLASSI_2,Freq,fill=CLASSI_2)) + geom_bar(stat = "identity")+
  labs(title = "Customer's distribution",size=18)+ylab("Percentuale clienti")+ scale_y_continuous(labels = percent) +
  scale_fill_manual(values=c("black","#2F4F4F","#801818","#CD7F32","#C0C0C0","gold","#B0E0E6"),guide=F)+
  theme_minimal()

RFM_2_plot

##################################################################################
##################################################################################