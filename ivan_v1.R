pacman::p_load(graphics, arules, arulesViz,readr, caret, corrplot, e1071, randomForest, mlbench, rstudioapi, dplyr, car, GGally, reshape)


# GITHUB SETUP:
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(current_path)


# IMPORTING DATA:
transactions <- read.transactions("~/Desktop/UBIQUM/Chapter 2/Task 4/datasets/ElectronidexTransactions2017.csv",sep = ",", header = FALSE)


# DATA INSPECTION:
inspect (transactions) # You can view the transactions. Is there a way to see a certain # of transactions?
length (transactions) # Number of transactions.
size (transactions) # Number of items per transaction
LIST(transactions) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(transactions)# To see the item labels


# PLOTS:
itemFrequencyPlot(transactions, topN = 20)
image(transactions[1:100,])
image(sample(transactions, 100))

