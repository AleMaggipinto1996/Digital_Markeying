############ MODELLO CHURN PER LE AZIENDE #############################

# Decidiamo la data di riferimento nel passato

# Il primo scontrino è stato emesso il 2018-05-01 e l'ultimo scontrino è stato emesso il 2019-04-30

df_7_aziende$TIC_DATE <- as.Date(df_7_aziende$TIC_DATE)
max(df_7_aziende$TIC_DATE)
min(df_7_aziende$TIC_DATE)

df_a <- df_7_aziende

df1_aziende <- df_a %>% 
  filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>1) #quanti acquisti per ogni cliente

df2_aziende <- df_a %>%
  filter(DIREZIONE==1) %>%
  group_by(ID_SCONTRINO) %>% 
  summarise(ID_CLI = max(ID_CLI),TIC_DATE=max(TIC_DATE))

df3_aziende <- left_join(df1_aziende,df2_aziende,by="ID_CLI") #aggiungiamo id_scontrino e la data

df4_aziende <- df3_aziende %>% 
  arrange(desc(TIC_DATE)) %>% 
  group_by(ID_CLI) %>% 
  summarise(last=nth(TIC_DATE,1),secondl=nth(TIC_DATE,2)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(last,secondl,units = "days")))


q_aziende <- ggplot(df4_aziende, aes(as.numeric(last-secondl), cumsum(stat(count)/nrow(df4_aziende)))) +
  geom_freqpoly(binwidth = 8,alpha=0.8,col="black") +
  labs(title = "Percentuale cumulativa di riacquisto", x = "days", y = "Cumulative Percentage of Repurchase") +
  geom_line(data = data.frame(days=1:365,const=0.80),aes(days,const),col="blue") +
  geom_line(data = data.frame(y=seq(0,1,0.1),x=68),aes(x,y),col="blue") +
  scale_x_continuous(breaks=seq(0,300,30)) +
  theme_classic()

q_aziende

# l'80% dei clienti riacquista entro 68 giorni

plot_a <- ggplot(df4_aziende, aes(x= DIFF_DAYS)) + 
  geom_histogram(color="#003399", fill="#99CBFF") +
  geom_vline(aes(xintercept = 72), color="#800000", linetype="dashed", size=1) +
  labs(title = "Ultimo acquisto - penultimo acquisto", x = "Intervallo di tempo", y = "Frequenza") +
  scale_x_continuous(breaks=seq(0,300,30)) +
  theme_minimal()

plot_a

# consideriamo un cliente come churn i clienti che non riacquistano entro tre mesi (decidiamo di prendere un campione leggermente più largo) ovvero nel periodo di holdout 
reference_date <- ymd(20190201)

#creiamo la colonna dei churner
df_churn_aziende <- df_7_aziende %>%
  filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarize(LAST_PURCHASE_DATE = max(TIC_DATE),
            TOTAL_PURCHASE = sum(IMPORTO_LORDO),
            NUMBER_OF_PURCHASE=n()) %>%
  mutate(CHURN = as.numeric(LAST_PURCHASE_DATE <= as.Date("2019-02-01"))) %>%
  select(CHURN,ID_CLI,LAST_PURCHASE_DATE,TOTAL_PURCHASE,NUMBER_OF_PURCHASE)

sum(df_churn_aziende$CHURN==1)
sum(df_churn_aziende$CHURN==0)

#10146 clienti non sono churn, 12152 sono churn

# the length of a holdout period after each reference date.

holdout_period_az <- df_7_aziende %>% 
  filter(TIC_DATE >= as.Date("2019-02-01")) %>%
  filter(DIREZIONE==1)                                               

#The holdout period starts on the 2019-02-01 and it ends at the 2019-04-30.

#Third STEP: Choosing the lenght of a lookback period before the reference date:
#consideriamo come lunghezza del periodo di lookback di cinque mesi

lookback_period_az  <- df_7_aziende %>% 
  filter(TIC_DATE < as.Date("2019-02-01") &TIC_DATE >= as.Date("2018-08-01") & DIREZIONE==1)


### scegliamo le variabili da inserire

#RECENCY:
Recency_churn_az <- lookback_period_az %>% 
  group_by(ID_CLI)%>%
  summarise(Last_date=max(TIC_DATE),
            Recency=as.numeric(difftime(reference_date,Last_date),units="days"))

#FREQUENCY:
Frequency_churn_az <- lookback_period_az %>% 
  group_by(ID_CLI) %>% summarise(Frequency=n_distinct(ID_SCONTRINO))

#MONETARY:
Monetary_churn_az <- lookback_period_az %>% 
  group_by(ID_CLI)%>%
  summarise(Imp_lordo = sum(IMPORTO_LORDO), 
            Sconto = sum(SCONTO), 
            SPESA_NETTA = Imp_lordo-Sconto) %>%
  select(ID_CLI, SPESA_NETTA)


#These variables concern the customer behaviour.
Churn_az <- merge(Recency_churn_az,Frequency_churn_az,by="ID_CLI")

Churn_az <- merge(Churn_az,Monetary_churn_az, by="ID_CLI")

df_churn2_az <- df_7_aziende %>%
  filter(TIC_DATE < as.Date("2019-02-01") &TIC_DATE >= as.Date("2018-08-01") & DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarize(NUMBER_OF_PURCHASE=n()) %>% 
  select(ID_CLI,NUMBER_OF_PURCHASE)


Churn2_az <- df_churn_aziende %>% select(ID_CLI, CHURN)

Churn2_az <- merge(df_churn2_az, Churn2_az, by= "ID_CLI")

Churn3_az <- merge(Churn_az, Churn2_az, by= "ID_CLI")

Churn_RFM_az <- RFM_aziende_1 %>% select(ID_CLI, CLASS_RF_aziende_1, CLASSI_aziende_1)

Churn_4_az <- Churn_RFM_az %>%  right_join (Churn3_az, by="ID_CLI")

Churn_4_az$CHURN <- as.factor(Churn_4_az$CHURN)



df_totale_az <- df_1_aziende %>% filter(LAST_DT_ACTIVE< as.Date("2019-02-01") & FIRST_DT_ACTIVE< as.Date("2019-02-01")) %>%
  select(ID_CLI, LAST_COD_FID, LAST_STATUS_FID, LAST_DT_ACTIVE) %>%
  left_join(df_2_aziende %>%
              select(ID_CLI, W_PHONE), by= "ID_CLI") %>%
  left_join(df_4_aziende, by = "ID_CLI")

df_finale_az <- merge(df_totale_az, Churn_4_az, by= "ID_CLI")

df_6_az <- df_6_aziende %>% filter(SEND_DATE< as.Date("2019-02-01") & OPENED == TRUE) %>% group_by(ID_CLI) %>%
  summarise(N_EMAIL_APERTE= n())

df_6_az2 <- df_6_aziende %>% filter(SEND_DATE< as.Date("2019-02-01") & CLICKED == TRUE) %>% group_by(ID_CLI) %>%
  summarise(N_EMAIL_CLICCATE= n())

df_finale2_az <- df_finale_az %>% left_join(df_6_az, by= "ID_CLI") %>% left_join(df_6_az2, by="ID_CLI")

df_rimborsi_az <- df_7_aziende %>%
  filter(TIC_DATE < as.Date("2019-02-01") &TIC_DATE >= as.Date("2018-08-01") & DIREZIONE== -1) %>%
  group_by(ID_CLI) %>%
  summarize(N_DI_RIMBORSI=n()) %>% 
  select(ID_CLI,N_DI_RIMBORSI)

df_finale_az <- df_finale2_az %>% left_join(df_rimborsi_az, by= "ID_CLI")

df_finale_az$N_DI_RIMBORSI[is.na(df_finale_az$N_DI_RIMBORSI)] <- 0
df_finale_az$N_EMAIL_APERTE[is.na(df_finale_az$N_EMAIL_APERTE)] <- 0
df_finale_az$N_EMAIL_CLICCATE[is.na(df_finale_az$N_EMAIL_CLICCATE)] <- 0
df_finale_az$W_PHONE[is.na(df_finale_az$W_PHONE)] <- 0

summary(df_finale_az)

df_finale_az <- df_finale_az %>% mutate(CLASS_RF_aziende_1 = fct_explicit_na(CLASS_RF_aziende_1, "Mancante"))

df_finale_az <- df_finale_az %>% mutate(CLASSI_aziende_1 = fct_explicit_na(CLASSI_aziende_1, "Mancante"))

df_finale_az <- df_finale_az %>% select(- ID_CLI)

#Train e Test set

train_index_az <- createDataPartition(df_finale_az$CHURN,p=0.70,list = FALSE, times=1)
train_az <- df_finale_az[train_index_az,]
test_az <- df_finale_az[-train_index_az,]

table(train_az$CHURN)

#il train è sbilanciato anche se di poco

ggplot(data=df_finale_az, aes(x=CHURN)) +
  geom_bar( fill="lightblue", color="black") +
  labs(x="No churn (0) / churn (1)", y="Number of Customers",
       title="Sbilanciamento Classi") +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

#The churn phenomenon is a classification problem with class imbalance.
#We need to solve classes imbalance problem

churn0_az <- train_az %>% filter(CHURN == 0) 
churn1_az <- train_az %>% filter(CHURN == 1) 

balance_az <- churn1_az[sample(nrow(churn1_az), nrow(churn0_az), replace = TRUE),] 
train_balanced_az <- rbind(balance_az, churn0_az)

train_az <- train_balanced_az
table(train_az$CHURN) 

#ora abbiamo una classe bilanciata


#1. RANDOM FOREST
memory.limit(100000)
tree_rf_az <- randomForest(CHURN~.,data=train_az,ntree=100)
print(tree_rf_az)

#Prediction Random Forest
pred_rf_az<-rpart.predict(tree_rf_az, test_az[,-15], type = "class")

prob_rf_az <- rpart.predict(tree_rf_az,test_az[,-15],type = "prob")[,1] 

cm_rf_az <- confusionMatrix(pred_rf_az, test_az$CHURN)
cm_rf_az

rec_rf_az <- recall(pred_rf_az,test_az$CHURN,relevant=1) #0.74

prec_rf_az <- precision(pred_rf_az,test_az$CHURN,relevant=1) #0.58

f1_rf_az <- F1_Score(pred_rf_az,test_az$CHURN, positive= 1) #0.73

acc_rf_az <- Accuracy(pred_rf_az,test_az$CHURN) #0.69

#___________________________________________________________________________________________________________________________________________________



#2. DECISION TREES

dec_tree_az<-rpart(CHURN~.,data = train_az)
rpart.plot(dec_tree_az)
summary(dec_tree_az)
printcp(dec_tree_az)


#prediction Decision Trees
pred_dt_az <- rpart.predict(dec_tree_az,test_az[,-15],type = "class") 

prob_dt_az <-rpart.predict(dec_tree_az,test_az[,-15],type = "prob")[,1]

cm_dt_az<-confusionMatrix(pred_dt_az,test_az$CHURN)

rec_dt_az <- recall(pred_dt_az,test_az$CHURN,relevant=1) #0.74

prec_dt_az <- precision(pred_dt_az,test_az$CHURN,relevant=1) #0.56

f1_dt_az <- F1_Score(pred_dt_az,test_az$CHURN, positive = 1) #0.71

acc_dt_az <- Accuracy(pred_dt_az,test_az$CHURN) #0.68


#___________________________________________________________________________________________________________________________________________________
#3. NAIVE BAYES

naive_az <- naiveBayes(CHURN~.,data = train_az)


#prediction Naive Bayes
pred_naive_az <- predict(naive_az,test_az[,-15])

prob_naive_az <- rpart.predict(naive_az,test_az[,-15], type = "raw")[,1]

cm_nb_az<-confusionMatrix(test_az$CHURN,pred_naive_az)
cm_nb_az

rec_nb_az <- recall(pred_naive_az,test_az$CHURN,relevant=1)# 0.50
prec_nb_az <- precision(pred_naive_az,test_az$CHURN,relevant=1)#0.65
f1_nb_az <- F1_Score(pred_naive_az,test_az$CHURN, positive = 1)#0.78
acc_nb_az <- Accuracy(pred_naive_az,test_az$CHURN) #0.71
#___________________________________________________________________________________________________________________________________________________

#4.LOGISTIC REGRESSION
gl_az <- glm(CHURN ~ ., train_az, family = "binomial")
summary(gl_az)

#non sono significative la variabile Recency, n_email_aperte, n_email_cliccate, il numero di rimborsi, Flag_privacy_1, il numero di telefono
train_az2 <- train_az %>% select(- Recency, -N_EMAIL_APERTE, -N_EMAIL_CLICCATE, -N_DI_RIMBORSI, - FLAG_PRIVACY_1, -W_PHONE)
test_az2 <- test_az %>% select(- Recency, -N_EMAIL_APERTE, -N_EMAIL_CLICCATE, -N_DI_RIMBORSI,- FLAG_PRIVACY_1, -W_PHONE)
gl2_az <- glm(CHURN~ ., train_az2, family = "binomial")
summary(gl2_az)

#prediction Logistic Regression
p1_az = predict(gl2_az, test_az2)
pred1_az = if_else(p1_az>0.5,1,0)
table_gl_az = table(pred1_az, test_az2$CHURN)
pred1_az <- as.factor(pred1_az)
confusionMatrix(table_gl_az)


log_az<-train(CHURN~.,data = train_az2,method = "glm")
prob_log_az<-predict(log,test_az2[,-12],type="prob")[,1]

#evaluate
rec_glm_az <- recall(pred1_az, test_az2$CHURN, relevant = "1") #0.44
prec_glm_az <- precision(pred1_az, test_az2$CHURN, relevant = "1") # 0.84
f1_glm_az <- F1_Score(pred1_az ,test_az2$CHURN,positive = '1') # 0.57
acc_glm_az <- Accuracy(pred1_az, test_az2$CHURN) #0.60



#___________________________________________________________________________________________________________________________________________________

#5.BAGGING

bag_az <- bagging(CHURN~.,data = train_az, nbagg=25)

#prediction Bagging

pred_bag_az<-predict(bag_az, test_az[,-15])

prob_bag_az <- rpart.predict(bag_az, test_az[,-15], type="prob")[,1]

cm_bag_az<-confusionMatrix(pred_bag_az, test_az$CHURN)

rec_bag_az <- recall(pred_bag_az, test_az$CHURN,relevant=1) #0.74

prec_bag_az <-  precision(pred_bag_az,test_az$CHURN, relevant=1) #0.56

f1_bag_az <- F1_Score(pred_bag_az, test_az$CHURN, positive = 1) #0.71

acc_bag_az <- Accuracy(pred_bag_az, test_az$CHURN) #0.67


#___________________________________________________________________________________________________________________________________________________



#The best modelling approach is identified by benchmarking the perfomance metrics precisions, recall, recall, F1-score, AUC and lift.

measure_matrix_az =matrix(0,ncol = 4,nrow = 5)
colnames(measure_matrix_az) <- c("Recall","Precision","F1_Score","Accuracy")
rownames(measure_matrix_az) <- c("Random forest","Decision trees",
                                 "Logistic regression","Naive bayes","Bagging")

measure_matrix_az[1,] <- c(rec_rf_az,prec_rf_az,f1_rf_az,acc_rf_az)
measure_matrix_az[2,] <- c(rec_dt_az,prec_dt_az,f1_dt_az,acc_dt_az)
measure_matrix_az[3,] <- c(rec_glm_az,prec_glm_az,f1_glm_az,acc_glm_az)
measure_matrix_az[4,] <- c(rec_nb_az,prec_nb_az,f1_nb_az,acc_nb_az)
measure_matrix_az[5,] <- c(rec_bag_az,prec_bag_az,f1_bag_az,acc_bag_az)
measure_df_az <- as.data.frame(measure_matrix_az)

#ACCURACY PLOT:
accuracy_df_az <- as.data.frame(cbind(rownames(measure_df_az),measure_df_az$Accuracy))

colnames(accuracy_df_az) <- c("Method","Value")

plot_accuracy_az<-ggplot(accuracy_df_az,aes(x=Method,y=Value,fill=Method)) +
  geom_bar(stat = "identity") + scale_fill_brewer(palette = "Reds")+
  ggtitle("ACCURACY PLOT")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

plot_accuracy_az #da aggiustare e fare più carino

#F1-score PLOT:
F1_score_df_az <- as.data.frame(cbind(rownames(measure_df_az),measure_df_az$F1_Score))
colnames(F1_score_df_az) <- c("Method","Value")

plot_f1_az<-ggplot(F1_score_df_az,aes(x=Method,y=Value,fill=Method)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Reds")+
  ggtitle("F1 SCORE PLOT")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))

plot_f1_az

#ROC CURVES:
roc_rf_az <- roc(test_az$CHURN ~ as.numeric(unlist(pred_rf_az)),plot=T,
                 print.auc=TRUE,col="blue",lwd =4,legacy.axes=TRUE)
roc_nb_az <- roc(test_az$CHURN ~ as.numeric(unlist(pred_naive_az)),plot=TRUE,
                 print.auc=TRUE,col="green",lwd = 4,print.auc.y=0.1,
                 legacy.axes=TRUE,add = TRUE)
roc_log_az <- roc(test_az$CHURN ~ as.numeric(unlist(pred1_az)),plot=TRUE,
                  print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.3,
                  legacy.axes=TRUE,add = TRUE)
roc_dt_az <- roc(test_az$CHURN ~ as.numeric(unlist(pred_dt_az)),plot=TRUE,
                 print.auc=TRUE,col="cyan",lwd = 4,print.auc.y=0.4,
                 legacy.axes=TRUE,add = TRUE)
roc_bag_az <- roc(test_az$CHURN ~ as.numeric(unlist(pred_bag_az)),plot=TRUE,
                  print.auc=TRUE,col="orange",lwd = 4,print.auc.y=0.2,
                  legacy.axes=TRUE,add = TRUE)

legend("right",legend=c("RF", "LOG", "DT", "NB","BAG"),fill =c("blue","red", "cyan", "green", "orange"), 
       cex = .75, inset = .1, bty = "n")


#######################################################################
#######################################################################
#######################################################################

library(funModeling)



#LIFT Measure per persone

lift_class <- as.data.frame(cbind(prob_bag, prob_dt, prob_naive, prob_rf, prob_log))
lift_class <- cbind(lift_class, test$CHURN)
colnames(lift_class)[6]="churn"

lift_bag <- gain_lift(data = lift_class, score ="prob_bag" , target = "churn" )
lift_dt <- gain_lift(data = lift_class, score ="prob_dt" , target = "churn" )
lift_naive <- gain_lift(data = lift_class, score ="prob_naive" , target = "churn" )
lift_rf <- gain_lift(data = lift_class, score ="prob_rf" , target = "churn" )
lift_log <- gain_lift(data = lift_class, score ="prob_log" , target = "churn" )


#LIFT Measure per aziende:

lift_class_az <- as.data.frame(cbind(prob_bag_az, prob_dt_az, prob_naive_az, prob_rf_az, prob_log_az))
lift_class_az <- cbind(lift_class_az, test_az$CHURN)
colnames(lift_class_az)[6]="churn"

lift_bag_az <- gain_lift(data = lift_class_az, score ="prob_bag_az" , target = "churn" )
lift_dt_az <- gain_lift(data = lift_class_az, score ="prob_dt_az" , target = "churn" )
lift_naive_az <- gain_lift(data = lift_class_az, score ="prob_naive_az" , target = "churn" )
lift_rf_az <- gain_lift(data = lift_class_az, score ="prob_rf_az" , target = "churn" )
lift_log_az <- gain_lift(data = lift_class_az, score ="prob_log_az" , target = "churn" )


#aggiustare grafici