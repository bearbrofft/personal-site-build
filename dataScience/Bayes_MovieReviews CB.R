# CLEAN UP CODE #
rm(list = ls())
gc()
cat("\f")

# SET WORKING DIRECTORY CODE #
setwd("C:/Users/cbrofft1/Desktop/AQM 2000/Some Course Files/Data")

# LOAD THE DATA
Moviesdf = read.csv('MovieReviews.csv')
library(scales)
library(e1071)
source('BabsonAnalytics.R')

# MANAGE THE DATA
Moviesdf$PositiveTweet = as.character(Moviesdf$PositiveTweet)
Moviesdf$PositiveTweet[Moviesdf$PositiveTweet == 1] = 'Positive'
Moviesdf$PositiveTweet[Moviesdf$PositiveTweet == 2] = 'Negative'
Moviesdf$X2 = NULL
Moviesdf$X25 = NULL
Moviesdf$X3 = NULL
Moviesdf[colnames(Moviesdf)] = lapply(Moviesdf[colnames(Moviesdf)], factor)

#PARTITION THE DATA
set.seed(1066) 
Split = 0.65 
N = nrow(Moviesdf) 
TrainingSize = round(N*Split) 
TrainingCases = sample(N, TrainingSize) 
Training = Moviesdf[TrainingCases, ] 
Test = Moviesdf[- TrainingCases, ] 

# BUILD OUR MODEL
Bayes = naiveBayes(PositiveTweet ~ ., data = Training)

# EVALUATE THE MODEL
Predictions = predict(Bayes, Test)
Actuals = Test$PositiveTweet
Benchmark = Training$PositiveTweet

Conf_matrix = table(Predictions, Actuals)

True_Pos = Conf_matrix[2, 2] 
True_Neg = Conf_matrix[1, 1] 
False_Neg = Conf_matrix[1, 2] 
False_Pos = Conf_matrix[2, 1]

Sensitivity = percent(True_Pos/(True_Pos + False_Neg), accuracy = 0.1) 
Specificity = percent(True_Neg/(True_Neg + False_Pos), accuracy = 0.1) 

Error_Rate = percent(sum(Predictions != Actuals)/length(Actuals), accuracy = 0.1) 
Error_Bench = percent(benchmarkErrorRate(Benchmark, Actuals), accuracy = 0.1) 

Bayes$tables$awesome
Bayes$tables$but
Bayes$tables$hate
Bayes$tables$mission

# CONDITIONAL PROBABILITY
Word = "mission"
Word_df = Training[ , c("PositiveTweet", Word)] 
Has_word = Word_df[Word_df[ , 2] == 1, ] 
Word_prob = sum(Has_word$PositiveTweet == "Positive") / nrow(Has_word) 
Word_prob = percent(Word_prob, accuracy = 0.1) 

cat("Given that a tweet contains the word \"", Word,"\", the probability the tweet is positive is ", Word_prob) 

