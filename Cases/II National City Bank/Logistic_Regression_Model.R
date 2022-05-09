# Logistic Regression

# Libs
library(dplyr)
library(vtreat)
library(pROC)
library(ggplot2)
library(MLmetrics)

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

# configure success variable as binary outcome
joinData$success <- cbind(joinData$success, ifelse(joinData$Y_AcceptedOffer == 'Accepted', 1, 0))
joinData$Y_AcceptedOffer <- NULL

# Informative and Target Variables
target <- 'success'
informativeVars <- c('PrevAttempts', 'past_Outcome', 'headOfhouseholdGender', 'carMake', 'carModel', 'carYr', 'annualDonations', 'AffluencePurchases', 'Age', 'Job', 'Marital', 'Education', 'DefaultOnRecord', 'RecentBalance', 'HHInsurance', 'CarLoan')

# Treatment Plan
plan <- designTreatmentsC(joinData, informativeVars, target, 1)
treatedData <- prepare(plan, joinData)

# Partitioning
split <- round(nrow(treatedData) %*% .8)
totalRecords <- 1:nrow(treatedData)
idx <- sample(totalRecords, split)

trainData <- treatedData[idx,]
testData <- treatedData[-idx,]

# Fit Model
fit <- glm(success ~., data = treatedData, family = 'binomial')

# Backward Variable selection
bestFit <- step(fit, direction='backward')
bestFit <- readRDS('bestFit.rds')

# Predict
predictions <- predict(bestFit, trainData, type='response')

# Classification (training Data)
### NOTE: I've set the cutoff to optimize accuracy for the question "who will accept the offer?".
### In an actual business case, we might use a function of the cost of contacting
### customers, the revenue from an accepted offer, and the odds the customer will accept.
cutoff <- 0.55
class <- ifelse(predictions >= cutoff, 1, 0)

# Reconcile predictions with original data
results <- data.frame(actual = trainData$success,
                      class = class,
                      probability = predictions)

# Confusion Matrix
confMat <- ConfusionMatrix(results$class, results$actual)
confMat

# Training Data Accuracy (75.98%)
sum(diag(confMat)) / sum(confMat)


# Test Data Classification
testPredictions <- predict(bestFit, testData, type='response')
testClass <- ifelse(testPredictions >= cutoff, 1, 0)

# Reconcile
testResults <- data.frame(actual = testData$success,
                      class = testClass,
                      probability = testPredictions)

# Confusion Matrix
testMat <- ConfusionMatrix(testResults$class, testResults$actual)
testMat

# Test Data Accuracy (76.13%)
sum(diag(testMat)) / sum(testMat)




### Run prospective customers Dataset to get top 100 customers

# Raw data
prospData   <- read.csv('ProspectiveCustomers.csv')

# Join Data
pData <- left_join(prospData, vehData, by = c('HHuniqueID'))
pData <- left_join(pData, axiomData, by = c('HHuniqueID'))
pData <- left_join(pData, creditData, by = c('HHuniqueID'))

# Apply treatment plan
pTreatedData <- prepare(plan, pData)

# add HHuniqueID back in for later identification
pTreatedData <- data.frame(pTreatedData, HHuniqueID = prospData$HHuniqueID)

# Predict
fullPredictions <- predict(bestFit, pTreatedData, type='response')
fullClass <- ifelse(fullPredictions >= cutoff, 1, 0)

# Reconcile
fullResults <- data.frame(class = fullClass,
                          probability = fullPredictions,
                          HHuniqueID = pTreatedData$HHuniqueID)

# Order results by probability
orderedResults <- fullResults %>% arrange(desc(fullResults$probability))

# Get the top 100 results
# (182 results with a probability of 1)
head(orderedResults, 100)

