##### MARKET BASKET ANALYSIS - PERSONE ##########

mba <- df_7_persone %>% filter(IMPORTO_LORDO > 0)
mba <- mba %>% 
            mutate(ID_TIC = paste0(mba$ID_CLI, "-", mba$TIC_DATETIME))%>%
            select(ID_TIC, ID_ARTICOLO)
            
mba$ID_TIC <- as.factor(mba$ID_TIC)
mba$ID_ARTICOLO <- as.factor(mba$ID_ARTICOLO)
write.table(mba, file = tmp <- file(), row.names = FALSE)

itemTrans <- read.transactions(tmp, format = "single",header = TRUE, cols = c("ID_TIC", "ID_ARTICOLO"))
close(tmp)

itemFrequencyPlot(itemTrans, topN=15, type="relative",col=brewer.pal(8,'Spectral'),main="Relative Item Frequency Plot")

rules <- apriori(itemTrans, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)

#The total numer of rules is 27
#A length of 3 items has the most rules (14) and length of 2 items have 
#the lowest number of rules (4)


inspect(rules)
# 95,6% of the customers who bought '{32078795,32079082,32842551} also bought {32079103}
ggplotly(plot(rules,method="two-key plot"))
#The above plot shows that rules with high confidence have low support.

topRules <- rules[1:10]

plot(topRules, method="graph",engine = "htmlwidget")
plot(topRules, method = "grouped")
