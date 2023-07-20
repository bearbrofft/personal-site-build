# CLEAN UP CODE #
rm(list = ls())
gc()
cat("\f")

######

# SET WORKING DIRECTORY CODE #
setwd("C:/Users/cbrofft1/Desktop/AQM 2000/Some Course Files/Data")

# LOAD THE DATA
source('BabsonAnalytics.R') 
library(scales) 
Housingdf = read.csv('BostonHousing.csv') 

# MANAGE THE DATA
Housingdf$ISHIGHVAL = as.logical(Housingdf$ISHIGHVAL)
Housingdf$MEDV = NULL 
Housingdf$CHAS = as.factor(Housingdf$CHAS) 
Housingdf$RAD = as.factor(Housingdf$RAD) 

# PARTITION THE DATA
set.seed(777) 
Split = 0.7 
N = nrow(Housingdf) 
TrainingSize = round(N*Split) 
TrainingCases = sample(N, TrainingSize) 
Training = Housingdf[TrainingCases, ] 
Test = Housingdf[- TrainingCases, ]

# BUILD THE MODEL
LogReg = glm(ISHIGHVAL ~ ., data = Training, family = 'binomial') 
summary(LogReg)
LogReg = step(LogReg, direction = 'backward')

# MAKE PREDICTIONS
Predictions = predict(LogReg, Test, type = 'response') 

# EVALUATE THE MODEL
PredTF = (Predictions > 0.5)
Actuals = Test$ISHIGHVAL 
Benchmark = Training$ISHIGHVAL 
table(PredTF, Actuals) 
ErrorRate = percent(sum(PredTF != Actuals)/length(Actuals), accuracy = 0.1) 
ErrorBench = percent(benchmarkErrorRate(Benchmark, Actuals), accuracy = 0.1)

True_Neg = sum(Actuals == FALSE & PredTF == FALSE)
True_Pos = sum(Actuals == TRUE & PredTF == TRUE)
False_Neg = sum(Actuals == TRUE & PredTF == FALSE)
False_Pos = sum(Actuals == FALSE & PredTF == TRUE)

Sensitivity = percent(True_Pos/(True_Pos + False_Neg), accuracy = 0.1)
Specificity = percent(True_Neg/(True_Neg + False_Pos), accuracy = 0.1)

# ROC CHART
ROCChart(Actuals, Predictions)

# LIFT CHART
liftChart(Actuals, Predictions)
