

####  CHURN PREDICTION MODEL

# Decidiamo la data di riferimento nel passato

# Il primo scontrino è stato emesso il 2018-05-01 e l'ultimo scontrino è stato emesso il 2019-04-30

df_7_persone$TIC_DATE <- as.Date(df_7_persone$TIC_DATE)
max(df_7_persone$TIC_DATE)
min(df_7_persone$TIC_DATE)

df <- df_7_persone

df1 <- df %>% 
  filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(N_SCONTRINI_PER_CLI= n_distinct(ID_SCONTRINO)) %>%
  filter(N_SCONTRINI_PER_CLI>1) #quanti acquisti per ogni cliente

df2 <- df %>%
  filter(DIREZIONE==1) %>%
  group_by(ID_SCONTRINO) %>% 
  summarise(ID_CLI = max(ID_CLI),TIC_DATE=max(TIC_DATE))

df3 <- left_join(df1,df2,by="ID_CLI") #aggiungiamo id_scontrino e la data

df4 <- df3 %>% 
  arrange(desc(TIC_DATE)) %>% 
  group_by(ID_CLI) %>% 
  summarise(last=nth(TIC_DATE,1),secondl=nth(TIC_DATE,2)) %>%
  mutate(DIFF_DAYS = as.numeric(difftime(last,secondl,units = "days")))


q <- ggplot(df4, aes(as.numeric(last-secondl), cumsum(stat(count)/nrow(df4)))) +
  geom_freqpoly(binwidth = 8,alpha=0.8,col="#84e4a8") +
  labs(x = "Days", y = "Cumulative Percentage of Repurchase") +
  geom_line(data = data.frame(days=1:365,const=0.80),aes(days,const),col="black") +
  geom_line(data = data.frame(y=seq(0,1,0.1),x=72),aes(x,y),col="black") +
  scale_x_continuous(breaks=seq(0,300,30)) +
  theme_classic()

q

# l'80% dei clienti riacquista entro 72 giorni

plot <- ggplot(df4, aes(x= DIFF_DAYS)) + 
  geom_histogram(color="#003399", fill="#99CBFF") +
  geom_vline(aes(xintercept = 72), color="#800000", linetype="dashed", size=1) +
  labs(title = "Ultimo acquisto - penultimo acquisto", x = "Intervallo di tempo", y = "Frequenza") +
  scale_x_continuous(breaks=seq(0,300,30)) +
  theme_minimal()

plot

# consideriamo un cliente come churn i clienti che non riacquistano entro tre mesi (decidiamo di prendere un campione leggermente più largo) ovvero nel periodo di holdout 
reference_date <- ymd(20190201)

#creiamo la colonna dei churner
df_churn <- df_7_persone %>%
  filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarize(LAST_PURCHASE_DATE = max(TIC_DATE),
            TOTAL_PURCHASE = sum(IMPORTO_LORDO),
            NUMBER_OF_PURCHASE=n())   %>%
  mutate(CHURN = as.numeric(LAST_PURCHASE_DATE <= as.Date("2019-02-01"))) %>%
  select(CHURN,ID_CLI,LAST_PURCHASE_DATE,TOTAL_PURCHASE,NUMBER_OF_PURCHASE)

sum(df_churn$CHURN==1)
sum(df_churn$CHURN==0)

#85829 clienti non sono churn, 103997 sono churn

# il periodo di holdout

holdout_period <- df_7_persone %>% 
  filter(TIC_DATE >= as.Date("2019-02-01")) %>%
  filter(DIREZIONE==1)                                               

#Il periodo di holdout dal 2019-02-01 al 2019-04-30.

#consideriamo come lunghezza del periodo di lookback di sei mesi

lookback_period  <- df_7_persone %>% 
  filter(TIC_DATE < as.Date("2019-02-01") &TIC_DATE >= as.Date("2018-08-01") & DIREZIONE==1)


### inseriamo le variabili di interesse

#RECENCY:
Recency_churn <- lookback_period%>% 
  group_by(ID_CLI)%>%
  summarise(Last_date=max(TIC_DATE),
            Recency=as.numeric(difftime(reference_date,Last_date),units="days"))

#FREQUENCY:
Frequency_churn <- lookback_period %>% 
  group_by(ID_CLI) %>% summarise(Frequency=n_distinct(ID_SCONTRINO))

#MONETARY:
Monetary_churn <- lookback_period %>% 
  group_by(ID_CLI)%>%
  summarise(Imp_lordo = sum(IMPORTO_LORDO), 
            Sconto = sum(SCONTO), 
            SPESA_NETTA = Imp_lordo-Sconto) %>%
  select(ID_CLI, SPESA_NETTA)


Churn <- merge(Recency_churn,Frequency_churn,by="ID_CLI")

Churn <- merge(Churn,Monetary_churn, by="ID_CLI")

df_churn2 <- df_7_persone %>%
  filter(TIC_DATE < as.Date("2019-02-01") &TIC_DATE >= as.Date("2018-08-01") & DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarize(NUMBER_OF_PURCHASE=n()) %>% 
  select(ID_CLI,NUMBER_OF_PURCHASE)


Churn2 <- df_churn %>% select(ID_CLI, CHURN)

Churn2 <- merge(df_churn2, Churn2, by= "ID_CLI")

Churn3 <- merge(Churn, Churn2, by= "ID_CLI")

Churn_RFM <- RFM_persone_1 %>% select(ID_CLI, CLASS_RF_persone_1, CLASSI_persone_1)

Churn_4 <- Churn_RFM %>%  right_join (Churn3, by="ID_CLI")

Churn_4$CHURN <- as.factor(Churn_4$CHURN)

df_totale <- df_1_persone %>% filter(LAST_DT_ACTIVE< as.Date("2019-02-01") & FIRST_DT_ACTIVE< as.Date("2019-02-01")) %>%
  select(ID_CLI, LAST_COD_FID, LAST_STATUS_FID, LAST_DT_ACTIVE) %>%
  left_join(df_2_persone %>%
              select(ID_CLI, W_PHONE), by= "ID_CLI") %>%
  left_join(df4_persone, by = "ID_CLI")

df_finale <- merge(df_totale, Churn_4, by= "ID_CLI")

df_6_p <- df_6_persone %>% filter(SEND_DATE< as.Date("2019-02-01") & OPENED == TRUE) %>% group_by(ID_CLI) %>%
  summarise(N_EMAIL_APERTE= n())

df_6_p2 <- df_6_persone %>% filter(SEND_DATE< as.Date("2019-02-01") & CLICKED == TRUE) %>% group_by(ID_CLI) %>%
  summarise(N_EMAIL_CLICCATE= n())

df_finale2 <- df_finale %>% left_join(df_6_p, by= "ID_CLI") %>% left_join(df_6_p2, by="ID_CLI")

df_rimborsi <- df_7_persone %>%
  filter(TIC_DATE < as.Date("2019-02-01") &TIC_DATE >= as.Date("2018-08-01") & DIREZIONE== -1) %>%
  group_by(ID_CLI) %>%
  summarize(N_DI_RIMBORSI=n()) %>% 
  select(ID_CLI,N_DI_RIMBORSI)

df_finale <- df_finale2 %>% left_join(df_rimborsi, by= "ID_CLI")

df_finale$N_DI_RIMBORSI[is.na(df_finale$N_DI_RIMBORSI)] <- 0
df_finale$N_EMAIL_APERTE[is.na(df_finale$N_EMAIL_APERTE)] <- 0
df_finale$N_EMAIL_CLICCATE[is.na(df_finale$N_EMAIL_CLICCATE)] <- 0

var_num = c("SPESA_NETTA", "NUMBER_OF_PURCHASE" ,"N_EMAIL_APERTE","N_EMAIL_CLICCATE","N_DI_RIMBORSI")

summary(df_finale)

df_finale <- df_finale %>% mutate(CLASS_RF_persone_1 = fct_explicit_na(CLASS_RF_persone_1, "Mancante"))

df_finale <- df_finale %>% mutate(CLASSI_persone_1 = fct_explicit_na(CLASSI_persone_1, "Mancante"))

df_finale <- df_finale %>% select(- ID_CLI)

#Train e Test set

train_index <- createDataPartition(df_finale$CHURN,p=0.70,list = FALSE, times=1)
train <- df_finale[train_index,]
test <- df_finale[-train_index,]

table(train$CHURN)

#il train è sbilanciato

ggplot(data=df_finale, aes(x=CHURN)) +
  geom_bar( fill="lightblue", color="black") +
  labs(x="No churn (0) / churn (1)", y="Number of Customers",
       title="Sbilanciamento Classi") +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

#dobbiamo risolvere il problema delle classi sbilanciate

churn0 <- train %>% filter(CHURN == 0) #contiene 54855 righe
churn1 <- train %>% filter(CHURN == 1) #contiene 33334 righe

balance <- churn1[sample(nrow(churn1), nrow(churn0), replace = TRUE),] 
train_balanced <- rbind(balance, churn0)

train <- train_balanced
table(train$CHURN) 

#ora abbiamo una classe bilanciata



#1. RANDOM FOREST
memory.limit(100000)
tree_rf <- randomForest(CHURN~.,data=train,ntree=100)
print(tree_rf)

#Prediction Random Forest
pred_rf<-rpart.predict(tree_rf, test[,-15], type = "class")

prob_rf <- rpart.predict(tree_rf,test[,-15],type = "prob")[,1]

cm_rf <- confusionMatrix(pred_rf, test$CHURN)
cm_rf

rec_rf <- recall(pred_rf,test$CHURN,relevant=1) #0.75

prec_rf <- precision(pred_rf,test$CHURN,relevant=1) #0.56

f1_rf <- F1_Score(pred_rf,test$CHURN, positive= 1) #0.72

acc_rf <- Accuracy(pred_rf,test$CHURN) #0.69

#___________________________________________________________________________________________________________________________________________________



#2. DECISION TREES

dec_tree<-rpart(CHURN~.,data = train)
rpart.plot(dec_tree)
summary(dec_tree) 
printcp(dec_tree)


#prediction Decision Trees
pred_dt <- rpart.predict(dec_tree,test[,-15],type = "class") 

prob_dt <-rpart.predict(dec_tree,test[,-15],type = "prob")[,1]

cm_dt<-confusionMatrix(pred_dt,test$CHURN)

rec_dt <- recall(pred_dt,test$CHURN,relevant=1) #0.71

prec_dt <- precision(pred_dt,test$CHURN,relevant=1) #0.55

f1_dt <- F1_Score(pred_dt,test$CHURN, positive = 1) #0.71

acc_dt <- Accuracy(pred_dt,test$CHURN) #0.67


#___________________________________________________________________________________________________________________________________________________
#3. NAIVE BAYES

naive <- naiveBayes(CHURN~.,data = train)


#prediction Naive Bayes
pred_naive <- predict(naive,test[,-15])

prob_naive <- rpart.predict(naive,test[,-15], type = "raw")[,1]

cm_nb<-confusionMatrix(test$CHURN,pred_naive)
cm_nb

rec_nb <- recall(pred_naive,test$CHURN,relevant=1)# 0.39
prec_nb <- precision(pred_naive,test$CHURN,relevant=1)#0.65
f1_nb <- F1_Score(pred_naive,test$CHURN, positive = 1)#0.78
acc_nb <- Accuracy(pred_naive,test$CHURN) #0.69
#___________________________________________________________________________________________________________________________________________________

#4.LOGISTIC REGRESSION
gl <- glm(CHURN ~ ., train, family = "binomial")
summary(gl)

#prediction Logistic Regression
p1 = predict(gl, test)
pred1 = if_else(p1>0.5,1,0)
table_gl = table(pred1, test$CHURN)
pred1 <- as.factor(pred1)
confusionMatrix(table_gl)


log<-train(CHURN~.,data = train,method = "glm")
prob_log<-predict(log,test[,-15],type="prob")[,1]

#evaluate
rec_glm <- recall(pred1, test$CHURN, relevant = "1") #0.38
prec_glm <- precision(pred1, test$CHURN, relevant = "1") # 0.85
f1_glm <- F1_Score(pred1 ,test$CHURN,positive = '1') # 0.53
acc_glm <- Accuracy(pred1, test$CHURN) #0.57



#___________________________________________________________________________________________________________________________________________________

#5.BAGGING

bag <- bagging(CHURN~.,data = train, nbagg=25)

#prediction Bagging

pred_bag<-predict(bag, test[,-15])

prob_bag <- rpart.predict(bag, test[,-15], type="prob")[,1]

cm_bag<-confusionMatrix(pred_bag, test$CHURN)

rec_bag <- recall(pred_bag, test$CHURN,relevant=1) #0.73

prec_bag <-  precision(pred_bag,test$CHURN, relevant=1) #0.54

f1_bag <- F1_Score(pred_bag, test$CHURN, positive = 1) #0.70

acc_bag <- Accuracy(pred_bag, test$CHURN) #0.67


#___________________________________________________________________________________________________________________________________________________

#il modello migliore viene definito dalle metrice: precisions, recall, recall, F1-score, AUC and lift.

measure_matrix=matrix(0,ncol = 4,nrow = 5)
colnames(measure_matrix) <- c("Recall","Precision","F1_Score","Accuracy")
rownames(measure_matrix) <- c("Random forest","Decision trees",
                              "Logistic regression","Naive bayes","Bagging")

measure_matrix[1,] <- c(rec_rf,prec_rf,f1_rf,acc_rf)
measure_matrix[2,] <- c(rec_dt,prec_dt,f1_dt,acc_dt)
measure_matrix[3,] <- c(rec_glm,prec_glm,f1_glm,acc_glm)
measure_matrix[4,] <- c(rec_nb,prec_nb,f1_nb,acc_nb)
measure_matrix[5,] <- c(rec_bag,prec_bag,f1_bag,acc_bag)
measure_df <- as.data.frame(measure_matrix)

#ACCURACY PLOT:
accuracy_df <- as.data.frame(cbind(rownames(measure_df),measure_df$Accuracy))

colnames(accuracy_df) <- c("Method","Value")

plot_accuracy<-ggplot(accuracy_df,aes(x=Method,y=Value,fill=Method)) +
  geom_bar(stat = "identity") + scale_fill_brewer(palette = "Reds")+
  ggtitle("ACCURACY PLOT")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(panel.border = element_blank())+theme(panel.background = element_blank())+theme(axis.line = element_line(colour = "black"))

plot_accuracy 

#F1-score PLOT:
F1_score_df <- as.data.frame(cbind(rownames(measure_df),measure_df$F1_Score))
colnames(F1_score_df) <- c("Method","Value")

plot_f1<-ggplot(F1_score_df,aes(x=Method,y=Value,fill=Method)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Reds")+
  ggtitle("F1 SCORE PLOT")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))

plot_f1

#ROC CURVES:
roc_rf <- roc(test$CHURN ~ as.numeric(unlist(pred_rf)),plot=T,
              print.auc=TRUE,col="blue",lwd =4,legacy.axes=TRUE)
roc_nb <- roc(test$CHURN ~ as.numeric(unlist(pred_naive)),plot=TRUE,
              print.auc=TRUE,col="green",lwd = 4,print.auc.y=0.1,
              legacy.axes=TRUE,add = TRUE)
roc_log <- roc(test$CHURN ~ as.numeric(unlist(pred1)),plot=TRUE,
               print.auc=TRUE,col="red",lwd = 4,print.auc.y=0.3,
               legacy.axes=TRUE,add = TRUE)
roc_dt <- roc(test$CHURN ~ as.numeric(unlist(pred_dt)),plot=TRUE,
              print.auc=TRUE,col="cyan",lwd = 4,print.auc.y=0.4,
              legacy.axes=TRUE,add = TRUE)
roc_bag <- roc(test$CHURN ~ as.numeric(unlist(pred_bag)),plot=TRUE,
               print.auc=TRUE,col="orange",lwd = 4,print.auc.y=0.2,
               legacy.axes=TRUE,add = TRUE)

legend("right",legend=c("RF", "LOG", "DT", "NB","BAG"),fill =c("blue","red", "cyan", "green", "orange"), 
       cex = .75, inset = .1, bty = "n")

#LIFT Measure per persone

lift_class <- as.data.frame(cbind(prob_bag, prob_dt, prob_naive, prob_rf, prob_log))
lift_class <- cbind(lift_class, test$CHURN)
colnames(lift_class)[6]="churn"

lift_bag <- gain_lift(data = lift_class, score ="prob_bag" , target = "churn" )
lift_dt <- gain_lift(data = lift_class, score ="prob_dt" , target = "churn" )
lift_naive <- gain_lift(data = lift_class, score ="prob_naive" , target = "churn" )
lift_rf <- gain_lift(data = lift_class, score ="prob_rf" , target = "churn" )
lift_log <- gain_lift(data = lift_class, score ="prob_log" , target = "churn" )
