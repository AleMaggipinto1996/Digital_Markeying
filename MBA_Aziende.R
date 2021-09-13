##### MARKET BASKET ANALYSIS - AZIENDE ##########

mba_az <- df_7_aziende %>% filter(IMPORTO_LORDO > 0)
mba_az <- mba_az %>% 
            mutate(ID_TIC = paste0(mba_az$ID_CLI, "-", mba_az$TIC_DATETIME))%>%
            select(ID_TIC, ID_ARTICOLO)
            
mba_az$ID_TIC <- as.factor(mba_az$ID_TIC)
mba_az$ID_ARTICOLO <- as.factor(mba_az$ID_ARTICOLO)
write.table(mba_az, file = tmp <- file(), row.names = FALSE)

itemTrans_az <- read.transactions(tmp, format = "single",header = TRUE, cols = c("ID_TIC", "ID_ARTICOLO"))
close(tmp)

itemFrequencyPlot(itemTrans_az, topN=15, type="relative",col=brewer.pal(8,'Spectral'),main="Relative Item Frequency Plot")

rules_az <- apriori(itemTrans_az, parameter = list(supp=0.001, conf=0.8))
rules_az <- sort(rules_az, by='confidence', decreasing = TRUE)

#The total numer of rules is 128


inspect(rules_az)
# 98,33% of the customers who bought '{32078795,32078935,32078970,32079082}
##also bought {32079103}
ggplotly(plot(rules_az,method="two-key plot"))
#The above plot shows that rules with high confidence have low support.

topRules_az <- rules_az[1:10]

plot(topRules_az, method="graph",engine = "htmlwidget")
plot(topRules_az, method = "grouped")
