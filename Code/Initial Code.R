library("pacman")
p_load(dplyr, ggplot2,corrplot, GGally, readr,caret,readxl,knitr,
       printr,party,polycor,BBmisc,car,reshape,arules,arulesViz,rstudioapi)


# GITHUB SETUP: 
current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)


# IMPORTING DATASET:
transactions <- read.transactions("Datasets/ElectronidexTransactions2017.csv", sep = ",",format = "basket",header = FALSE)


# DATA INSPECTION:
itemLabels(transactions)
length (transactions)
inspect(transactions[1:10], itemSep = " + ", setStart = "",
        setEnd ="", linebreak = FALSE)
size (transactions) # Number of items per transaction
LIST(transactions) # Lists the transactions by conversion 


# PLOTS:
itemFrequencyPlot(transactions, horiz = TRUE, 
                  type = "absolute",topN = 20,popCol = TRUE)
image(sample(transactions, 100))


#Creating rules for the transactions
Rules <- apriori (transactions, parameter = list(supp = 0.03, 
                  conf = 0.2,minlen = 2,target = "rules"))
inspect(sort(Rules,by = "lift"))
summary(Rules)
imacrules <- subset(Rules, items %in% "iMac")
inspect(imacrules)
plot(Rules)
