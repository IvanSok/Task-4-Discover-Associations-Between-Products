library("pacman")
p_load(dplyr, ggplot2,corrplot, GGally, readr,caret,readxl,knitr,
       printr,party,polycor,BBmisc,car,reshape,arules,arulesViz,rstudioapi)


# GITHUB SETUP: 
current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)

# IMPORTING DATASET:
transactions <- read.transactions("Datasets/ElectronidexTransactions2017.csv",rm.duplicates = FALSE,
                                  sep = ",",format = "basket")
transactionsdf <- read_csv("Datasets/ElectronidexTransactions2017.csv", 
                                                           +     col_names = FALSE)

itemlevels <- read.csv("Datasets/ItemLevels.csv", sep = ";",header = FALSE, colClasses = 'character')

# DATA INSPECTION:
itemLabels(transactions)
length (transactions)
inspect(transactions[1:10], itemSep = " + ", setStart = "",
        setEnd ="", linebreak = FALSE)
size (transactions[10:20]) # Number of items per transaction
LIST(transactions[10:20]) # Lists the transactions by conversion 


# PLOTS:
itemFrequencyPlot(transactions, horiz = TRUE, 
                  type = "absolute",topN = 20,popCol = TRUE)
image(sample(transactions, 100))


#Creating rules for the transactions
rules <- apriori (transactions, parameter = list(supp = 0.0015, 
                                                 conf = 0.6,minlen = 2,target = "rules"))
rules <- rules[which(is.redundant(rules) == FALSE)]
inspect(sort(rules,by = "lift"))
ruleExplorer(rules)
summary(rules)
plot(rules)

# SORTING RULES BY:
rules_list <- c("lift", "support", "confidence")
j <- "## Sorted by"

for (i in rules_list){
  p <- paste(j,i)
  print(p)
  inspect(sort(rules, by = i))
  
}

#Loop to get rules for every subset
itemrules <- list()
rules_loop <- c()
for (k in itemLabels(transactions)) {
  rules_loop <- subset(rules, items %in% k)
  itemrules[[k]] <- rules_loop
}
inspect(itemrules$iMac)

saveRDS(object = itemrules,file = "Models/ItemRulesSubset")

inspectDT(rules)

itemmatrix <- as(transactions,"matrix")

itemlevels <- read.csv("Datasets/ItemLevels.csv", sep = ";",header = FALSE, colClasses = 'character')

itemlevels <- reorder(itemlevels)
sghaiue[1,25] <- head(transactionsdf, 30)
which(base::duplicated(transactionsdf,incomparables = "NA") == TRUE)
