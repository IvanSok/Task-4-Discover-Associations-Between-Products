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
transactionsdf <- read.csv("Datasets/ElectronidexTransactions2017.csv",header = FALSE,colClasses = 'character')

itemlevels <- read.csv("Datasets/ItemLevels.csv", sep = ";",header = FALSE, colClasses = 'character')

# DATA INSPECTION:
itemLabels(transactions)
length (transactions)
inspect(transactions[1:10], itemSep = " + ", setStart = "",
        setEnd ="", linebreak = FALSE)
cat("Number of transactions per item:")
for(l in 1:max(size(transactions))){

  print(paste(l,"Item -->", length(which(size(transactions)== l))))
   
}

# PLOTS:
itemFrequencyPlot(transactions, horiz = TRUE, 
                  type = "absolute",topN = 20,popCol = TRUE)
image(sample(transactions, 100))


#Creating rules for the transactions
rules <- apriori (transactions, parameter = list(supp = 0.0015, 
                                                 conf = 0.8, minlen = 2,target = "rules"))
rules <- rules[which(is.redundant(rules) == FALSE)]
ruleExplorer(rules)

# SORTING RULES BY:
rules_list <- c("lift", "support", "confidence")

for (i in rules_list){
  cat("\nRules Sorted by",i,"\n")
  inspect(head(sort(rules, by = i,decreasing = TRUE),n = 10)
          ,itemSep = " + ", setStart = "",setEnd ="", linebreak = FALSE)
}

#Loop to get rules for every subset
itemrules <- list()
rules_loop <- c()
for (k in itemLabels(transactions)) {
  rules_loop <- subset(rules, items %in% k)
  itemrules[[k]] <- rules_loop
}
#inspect(itemrules$iMac)
saveRDS(object = itemrules,file = "Models/ItemRulesSubset")

itemmatrix <- as(transactions,"matrix")