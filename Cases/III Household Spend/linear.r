# Linear Regression

## Libs
library(caret)
library(e1071)
library(plyr)
library(dplyr)
library(class)
library(MLmetrics)

# Wd
setwd("~/Desktop/Andrew_Harvard_DataMining_Business/Cases/III Household Spend")

# Import Training Data
consumerData <- read.csv('studentTables/training/consumerData_training15K_studentVersion.csv')
donationsData <- read.csv('studentTables/training/DonationsData_training15K_studentVersion.csv')
magazineData <- read.csv('studentTables/training/magazineData_training15K_studentVersion.csv')
politicalData <- read.csv('studentTables/training/politicalData_training15K_studentVersion.csv')
inHouseData <- read.csv('studentTables/training/inHouseData_training15K_studentVersion.csv')

# Joins
trainData <- left_join(inHouseData, donationsData, by = c('tmpID'))
trainData <- left_join(trainData, magazineData, by = c('tmpID'))
trainData <- left_join(trainData, politicalData, by = c('tmpID'))
trainData <- left_join(trainData, consumerData, by = c('tmpID'))

# exclude NAs
trainData <- na.exclude(trainData)

# Informative Variables
relevantVars <- c('state', 'PropertyType', 'DonationScore', 
                     "FamilyMagazineInHome", "FemaleOrientedMagazineInHome", 
                     "GardeningMagazineInHome", "CulinaryInterestMagazineInHome", 
                     "DoItYourselfMagazineInHome", "OccupationIndustry",
                     'UpscaleBuyerInHome', 'ComputerOwnerInHome')

features <- setdiff(relevantVars, 'yHat')

# Treatment Plan
plan <- designTreatmentsZ(trainData, features, verbose = FALSE)
treatedTrain <- prepare(plan, trainData)
treatedTrain$yHat <- trainData$yHat

# Fit
fit <- lm(yHat ~ ., treatedTrain)

# Backfit 
backFit <- step(fit,direction = 'backward', trace = 5)

# Training Set Predictions
trainingPreds <- predict(backFit, treatedTrain)

#Organize training set preds
trainingResults <-data.frame(actuals        = treatedTrain$yHat,
                             predicted      = trainingPreds,
                             residualErrors = treatedTrain$yHat-trainingPreds)

# RMSE - 75.37
trainRMSE <- MLmetrics::RMSE(trainingResults$predicted, 
                              trainingResults$actuals)

# MAPE - 0.29
trainMAPE <- MLmetrics::MAPE(trainingResults$predicted, 
                              trainingResults$actuals)
#################
### TEST DATA ###
#################

# Import Training Data
consumerTestData <- read.csv('studentTables/testing/consumerData_testing5K_studentVersion.csv')
donationsTestData <- read.csv('studentTables/testing/DonationsData_testing5K_studentVersion.csv')
magazineTestData <- read.csv('studentTables/testing/magazineData_testing5K_studentVersion.csv')
politicalTestData <- read.csv('studentTables/testing/politicalData_testing5K_studentVersion.csv')
inHouseTestData <- read.csv('studentTables/testing/inHouseData_testing5K_studentVersion.csv')

# Joins
testData <- left_join(inHouseTestData, donationsTestData, by = c('tmpID'))
testData <- left_join(testData, magazineTestData, by = c('tmpID'))
testData <- left_join(testData, politicalTestData, by = c('tmpID'))
testData <- left_join(testData, consumerTestData, by = c('tmpID'))

# exclude NAs
testData <- na.exclude(testData)

# Apply treatment plan
treatedTest <- prepare(plan, testData)
treatedTest$yHat <- testData$yHat

# Predict
testPreds   <- predict(backFit, treatedTest) 

testResults <- data.frame(actuals   = treatedTest$yHat,
                          predicted = testPreds)

# RMSE - 76.23
testRMSE <- MLmetrics::RMSE(testResults$predicted, 
                             testResults$actuals)

# MAPE - 0.29
testMAPE <- MLmetrics::MAPE(testResults$predicted, 
                             testResults$actuals)

