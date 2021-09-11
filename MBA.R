install.packages("arulesViz")
library(arules)
library(arulesViz)
library(plyr)
library(RColorBrewer)

data_mba <- df_7_persone %>% filter(IMPORTO_LORDO > 0) 
itemList <- ddply(data_mba,c("ID_CLI","TIC_DATETIME"),function(df1)paste(df1$ID_ARTICOLO, collapse = ","))
itemList$ID_CLI <- NULL
itemList$TIC_DATETIME <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN=15, type="relative",col=brewer.pal(8,'Spectral'),main="Relative Item Frequency Plot")

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
#The total numer of rules is 27
#A length of 3 items has the most rules (14) and length of 2 items have 
#the lowest number of rules (4)


inspect(rules)
# 95,6% of the customers who bought '{32078795,32079082,32842551} also bought {32079103}
ggplotly(plot(rules,method="two-key plot"))
#The above plot shows that rules with high lift have low support.

topRules <- rules[1:10]

plot(topRules, method="graph",engine = "htmlwidget")
plot(topRules, method = "grouped")
