# KNN

## Libs
library(caret)
library(e1071)
library(dplyr)
library(class)

# WD
setwd("~/Desktop/Andrew_Harvard_DataMining_Business/Cases/II National City Bank")

# Raw data
currentData   <- read.csv('training/CurrentCustomerMktgResults.csv')
vehData <- read.csv('training/householdVehicleData.csv') 
axiomData <- read.csv('training/householdAxiomData.csv')
creditData <- read.csv('training/householdCreditData.csv')

# Join Data
joinData <- left_join(currentData, vehData, by = c('HHuniqueID'))
joinData <- left_join(joinData, axiomData, by = c('HHuniqueID'))
joinData <- left_join(joinData, creditData, by = c('HHuniqueID'))

# exclude NAs
joinData <- na.exclude(joinData)

# Partitioning
split <- round(nrow(joinData) %*% .8)
totalRecords <- 1:nrow(joinData)
idx <- sample(totalRecords, split)

trainData <- joinData[idx,]
testData <- joinData[-idx,]

# Fit the model
knnFit <- train(Y_AcceptedOffer ~.,
                data = trainData,
                method = 'knn',
                preProcess = c('center', 'scale'),
                tuneLength = 10)

# Evaluate Model
knnFit
plot(knnFit)

# Accuracy = 54.55%
### This is much worse than the other models.  No need to continue.


