# CLEAN UP CODE #
rm(list = ls())
gc()
cat("\f")

######

# SET WORKING DIRECTORY CODE #
setwd("C:/Users/cbrofft1/Desktop/AQM 2000/Some Course Files/Data")

# LOAD THE DATA
Toytoadf = read.csv('ToyotaCorolla.csv')

# MANAGE THE DATA
Toytoadf$Fuel_Type = as.factor(Toytoadf$Fuel_Type)
Toytoadf$Met_Color = as.logical(Toytoadf$Met_Color)
Toytoadf$Automatic = as.logical(Toytoadf$Automatic)
Toytoadf$Model = NULL

# PARTITION THE DATA
set.seed(5678) #change this
N = nrow(Toytoadf) #change this
Split = 0.7 #change this
TrainingSize = round(N * Split)
TrainingCases = sample(N, TrainingSize)
Training = Toytoadf[TrainingCases, ] #change df
Test = Toytoadf[-TrainingCases, ] # change df

# BUILD THE MODEL
options(scipen = 3) #adjust to organize summary output
LinReg = lm(Price ~., data = Training)
summary(LinReg)

# REFINE THE MODEL
Backward = step(LinReg, direction = 'backward')
Backward$anova
Backward$coefficients

Intercept_only = lm(Price ~ 1, data = Training)
Intercept_only$coefficients

All = lm(Price ~ ., data = Training)

Forward = step(Intercept_only, direction = 'forward', scope = formula(All))

Both = step(Intercept_only, direction = 'both', scope = formula(All))

# MAKE PREDICTIONS
LinRegBest = Backward
Predictions = predict(LinRegBest, Test)
Actuals = Test$Price
Errors = Actuals - Predictions

# EVALUATE THE MODEL
RMSE = sqrt(mean(Errors^2))
MAPE = mean(abs(Errors/Actuals))
library(scales)
MAPE = percent(MAPE, accuracy = 0.1)

Benchmark = mean(Training$Price)
Errors_bench = Actuals - Benchmark
