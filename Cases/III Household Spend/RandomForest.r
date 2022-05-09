# Random Forest

## Libs
library(caret)
library(e1071)
library(plyr)
library(dplyr)
library(class)
library(randomForest)
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
Fit <- train(yHat ~ .,
             data = treatedTrain,
             method = "rf",
             verbose = FALSE,
             ntree = 200,
             tuneGrid = data.frame(mtry = 1)) #num of vars used in each tree
Fit

# ntree = 100
# RMSE - 89.98
# Rsquared - .34
# MAE - 71.78

# ntree = 200
# RMSE - 89.73
# Rsquared - .34
# MAE - 71.80

# ntree = 300
# RMSE - 89.85
# Rsquared - .34
# MAE - 71.8

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