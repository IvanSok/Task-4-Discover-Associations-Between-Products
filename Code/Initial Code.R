library("pacman")
p_load(dplyr, ggplot2,corrplot, GGally, readr,caret,readxl,knitr,
       printr,party,polycor,BBmisc,car,reshape,arules,arulesViz)

transactions <- read.transactions("~/Ubiqum/Task 6/ElectronidexTransactions2017.csv", sep = ",",format = "basket",header = FALSE)



itemLabels(transactions)
length (transactions)
inspect(transactions[1:10], itemSep = " + ", setStart = "", setEnd ="", linebreak = FALSE)

itemFrequencyPlot(transactions, horiz = TRUE, type = "absolute",topN = 20,popCol = TRUE)

apriori(transactions)
Rules<- apriori (transactions, parameter = list(supp = 0.6, conf = 0.8,minlen=2,maxlen=30))
inspect(Rules)

