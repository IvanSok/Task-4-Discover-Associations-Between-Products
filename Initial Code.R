library("pacman")
p_load(dplyr, ggplot2,corrplot, GGally, readr,caret,readxl,knitr,
       printr,party,polycor,BBmisc,car,reshape,arules,arulesViz,rstudioapi)


# GITHUB SETUP: 
current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)

# IMPORTING DATASET:
transactions <- read.transactions("ElectronidexTransactions2017.csv",rm.duplicates = FALSE,
                                  sep = ",",format = "basket")
transactionsdf <- read.csv("ElectronidexTransactions2017.csv",header = FALSE,colClasses = 'character')

itemlevels <- read.csv("ItemLevels.csv", sep = ";",header = FALSE, colClasses = 'character')

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
#ruleExplorer(rules)
ruleExplorer(rules)
# SORTING RULES BY:
rules_list <- c("lift", "support", "confidence")

for (i in rules_list){
  cat("\nRules Sorted by",i,"\n")
  inspect(head(sort(rules, by = i,decreasing = TRUE),n = 10)
          ,itemSep = " + ", setStart = "",setEnd ="", linebreak = FALSE)
}

transactions@itemInfo$category <- itemlevels[,2]
transactions_divided <- aggregate(transactions, by = 'category')
itemLabels(transactions_divided)
itemFrequencyPlot(transactions_divided, horiz = TRUE, 
                  type = "absolute",topN = 20,popCol = TRUE)
rules_divided <- apriori (transactions_divided, parameter = list(supp = 0.01, 
                                                 conf = 0.8, minlen = 2,target = "rules"))

rules_divided <- rules_divided[which(is.redundant(rules_divided) == FALSE)]
#Loop to get rules_divided for every subset
itemrules <- list()
rules_loop <- c()
for (k in itemLabels(transactions_divided)) {
  rules_loop <- subset(rules_divided, items %in% k)
  itemrules[[k]] <- rules_loop
  }
saveRDS(object = itemrules,file = "Models/ItemRulesSubset")
transactions@itemInfo$labels <- paste(itemlevels[,2],itemLabels(transactions))
divided_df <- as.data.frame(as(transactions, 'matrix'))
divided_df <- as.data.frame(ifelse(test = divided_df == TRUE,yes = 1,no = 0))
x <- names(divided_df)
for (i in 1:ncol(divided_df)){
  for (j in 1:nrow(divided_df)){
    if (divided_df[j,i]==1){
      divided_df[j,i] <- x[i]
      }
   }
}
for (i in 1:nrow(divided_df)) {
  vLaptops <- sum(grepl(pattern = "Laptops",x = divided_df[i,]))
  vDesktop <- sum(grepl(pattern = "Desktop",x = divided_df[i,]))
  vPrinters <- sum(grepl(pattern = "Printers",x = divided_df[i,]))
  if(vLaptops>=2 | vDesktop>=2 | vPrinters>=2){
    divided_df$Retail[i] <- "Company" } else {divided_df$Retail[i] <- "Costumer"}
}

paste(round((length(which(divided_df$Retail == "Costumer")))/nrow(divided_df)*100,digits = 2),
          "%"," of transactions are done by Costumers",sep = "")
paste(round((length(which(divided_df$Retail == "Company")))/nrow(divided_df)*100,digits = 2),
      "%"," of transactions are done by Companies",sep = "")
