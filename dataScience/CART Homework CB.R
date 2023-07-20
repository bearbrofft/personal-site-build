# CLEAN UP CODE #
rm(list = ls())
gc()
cat("\f")

# SET WORKING DIRECTORY CODE #
setwd("C:/Users/cbrofft1/Desktop/AQM 2000/Some Course Files/Data")

# LOAD THE DATA
Bankdf = read.csv('UniversalBank.csv')
source('BabsonAnalytics.R')
library(scales)
library(e1071)
library(rpart.plot)
library(rpart)

# MANAGE THE DATA
Bankdf$ZIP.Code = NULL
Bankdf$ID = NULL
Bankdf$Personal.Loan[Bankdf$Personal.Loan == 0] = 'Decline'
Bankdf$Personal.Loan[Bankdf$Personal.Loan == 1] = 'Accept'
Bankdf$Education[Bankdf$Education == 1] = 'UGrad'
Bankdf$Education[Bankdf$Education == 2] = 'Grad'
Bankdf$Education[Bankdf$Education == 3] = 'Prof'
Bankdf$Securities.Account[Bankdf$Securities.Account == 0] = 'No'
Bankdf$Securities.Account[Bankdf$Securities.Account == 1] = 'Yes'
Bankdf$CD.Account[Bankdf$CD.Account == 0] = 'No'
Bankdf$CD.Account[Bankdf$CD.Account == 1] = 'Yes'
Bankdf$Online[Bankdf$Online == 0] = 'No'
Bankdf$Online[Bankdf$Online == 1] = 'Yes'
Bankdf$CreditCard[Bankdf$CreditCard == '0'] = 'No'
Bankdf$CreditCard[Bankdf$CreditCard == '1'] = 'Yes'
cols = c("Education", 'Personal.Loan', "Securities.Account", 'CD.Account', 'Online', 'CreditCard')
Bankdf[cols] = lapply(Bankdf[cols], factor)

# PARTITION THE MODEL
set.seed(123456) 
Split = 0.60 
N = nrow(Bankdf) 
TrainingSize = round(N*Split) 
TrainingCases = sample(N, TrainingSize) 
Training = Bankdf[TrainingCases, ] 
Test = Bankdf[- TrainingCases, ] 

# DEFAULT TREE
Default_Tree = rpart(Personal.Loan ~ ., Training) 
rpart.plot(Default_Tree, main = "Default Tree", digits = 3)
Actuals = Test$Personal.Loan 
Predictions = predict(Default_Tree, Test, type = "class")
Benchmark = Training$Personal.Loan 
ErrorBench = percent(benchmarkErrorRate(Benchmark, Actuals), accuracy = 0.1)
ErrorRateDefault = percent(sum(Predictions != Actuals)/length(Actuals), accuracy = 0.1)

# Full Grown Tree
StoppingRules = rpart.control(minsplit = 2, minbucket = 1, cp = -1) 
Full_Tree = rpart(Personal.Loan ~ ., Training, control = StoppingRules) 
rpart.plot(Full_Tree, main = "Full Grown Tree", digits = 3) 
Predictions = predict(Full_Tree, Test, type = "class") 
ErrorRateFull = percent(sum(Predictions != Actuals)/length(Actuals), accuracy = 0.1) 

# Pruning the Tree
Pruned = easyPrune(Full_Tree) 
rpart.plot(Pruned, main = "Pruned Tree", digits = 3) 
Predictions = predict(Pruned, Test, type = "class") 
ErrorRatePruned = percent(sum(Predictions != Actuals)/length(Actuals), accuracy = 0.1) 

