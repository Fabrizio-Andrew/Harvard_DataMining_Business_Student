#' Case II Supplemental
#' TK
#' 4-30

# Libs
library(dplyr)
library(vtreat)
library(caret)

# Wd
setwd("~/Desktop/Andrew_Harvard_DataMining_Business/Cases/II National City Bank")

# Raw data
currentData   <- read.csv('training/CurrentCustomerMktgResults.csv')
vehData <- read.csv('training/householdVehicleData.csv') 
axiomData <- read.csv('training/householdAxiomData.csv')
creditData <- read.csv('training/householdCreditData.csv')

# Perform a join, neeed to add other data sets
joinData <- left_join(currentData, vehData, by = c('HHuniqueID'))
joinData <- left_join(joinData, axiomData, by = c('HHuniqueID'))
joinData <- left_join(joinData, creditData, by = c('HHuniqueID'))

# This is a classification problem so ensure R knows Y isn't 0/1 as integers
joinData$Y_AcceptedOffer <- as.factor(joinData$Y_AcceptedOffer)

## SAMPLE: Partition schema
set.seed(1234)
idx       <- ...
trainData <- ...
testData  <- ...

## EXPLORE: EDA, perform your EDA

## MODIFY: Vtreat, need to declare xVars & name of Y var
xVars <- c('DaysPassed', 'Communication', 'Outcome', ...)
yVar  <- '...'
plan  <- designTreatmentsC(..., xVars, ..., 1)

# Apply the rules to the set
treatedTrain <- prepare(..., trainData)
treatedTest  <- prepare(plan, ...)

## MODEL: caret etc.
fit <- train(Y_AcceptedOffer ~., data = ..., method = ...)

## ASSESS: Predict & calculate the KPI appropriate for classification
trainingPreds <- predict(..., ...)
testingPreds  <- predict(..., ...)

## NOW TO GET PROSPECTIVE CUSTOMER RESULTS
# 1. Load Raw Data
prospects <- read.csv('/cloud/project/cases/National City Bank/ProspectiveCustomers.csv')

# 2. Join with external data

# 3. Apply a treatment plan

# 4. Make predictions
prospectPreds <- predict(..., treatedProspects, type= 'prob')

# 5. Join probabilities back to ID
prospectsResults <- cbind(prospects$HHuniqueID, ...)

# 6. Identify the top 100 "success" class probabilities from prospectsResults


# End
