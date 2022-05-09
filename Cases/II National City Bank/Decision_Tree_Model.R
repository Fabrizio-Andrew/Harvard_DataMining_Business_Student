# Decision Tree

# Libs
library(caret)
library(rpart.plot)
library(dplyr)


# Wd
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

# Exclude unique vars with no correlation
uniqueVars <- c('HHuniqueID', 'CallStart', 'CallEnd', 'carMake', 'carModel', 'EstRace', 'annualDonations')

# fit the model
fit <- train(as.factor(Y_AcceptedOffer) ~.,
            data = trainData[,!colnames(trainData) %in% uniqueVars],
            method = 'rpart',
            tuneGrid = data.frame(cp = c(0.0001, 0.001, 0.01, 0.05, 0.075, 0.1)),
            control = rpart.control(minsplit = 1, minbucket = 2))
fit

# Plot the tree
prp(fit$finalModel, extra = 1)

# training set predictions
trainCaret <- predict(fit, trainData)

# Confusion Matrix
confMat <- confusionMatrix(trainCaret, as.factor(trainData$Y_AcceptedOffer))
confMat

# Training Data Accuracy = 75.83%


# Test Data Predictions
testCaret <- predict(fit, testData)

# Test Data Confusion Matrix
testMat <- confusionMatrix(testCaret, as.factor(testData$Y_AcceptedOffer))
testMat

# Test Data Accuracy = 71.68%

