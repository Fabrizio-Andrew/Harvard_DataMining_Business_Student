# KNN

## Libs
library(caret)
library(e1071)
library(plyr)
library(dplyr)
library(class)

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

# Create Donation Score
trainData$score1 <- ifelse(trainData$DonatesToCharityInHome == "Yes", 1, 0)
trainData$score2 <- ifelse(trainData$DonatestoAnimalWelfare == "Yes", 1, 0)
trainData$score3 <- ifelse(trainData$DonatestoChildrensCauses == "Yes", 1, 0)
trainData$score4 <- ifelse(trainData$DonatestoHealthcare == "Yes", 1, 0)
trainData$score5 <- ifelse(trainData$DonatestoInternationalAidCauses == "Yes", 1, 0)
trainData$score6 <- ifelse(trainData$DonatestoVeteransCauses == "Yes", 1, 0)
trainData$score7 <- ifelse(trainData$DonatestoArtsandCulture == "Yes", 1, 0)
trainData$score8 <- ifelse(trainData$DonatestoWildlifePreservation == "Yes", 1, 0)
trainData$score9 <- ifelse(trainData$DonatestoLocalCommunity == "Yes", 1, 0)

trainData$donationScore <- trainData$score1 + 
  trainData$score2 +
  trainData$score3 +
  trainData$score4 +
  trainData$score5 +
  trainData$score6 +
  trainData$score7 +
  trainData$score8 +
  trainData$score9

# Restricting to Informative Variables (from EDA) to improve performance
informativeVars <- c('state', 'PropertyType', 'donationScore', 
                     "FamilyMagazineInHome", "FemaleOrientedMagazineInHome", 
                     "GardeningMagazineInHome", "CulinaryInterestMagazineInHome", 
                     "DoItYourselfMagazineInHome", "OccupationIndustry",
                     'UpscaleBuyerInHome', 'ComputerOwnerInHome')

infData <- trainData[informativeVars]
infData$yHat <- trainData$yHat

# Fit the model
knnFit <- train(yHat ~.,
                data = infData,
                method = 'knn',
                preProcess = c('center', 'scale'),
                tuneLength = 5)

# Evaluate Model
knnFit
plot(knnFit)

# k   RMSE      Rsquared   MAE     
# 5   92.94857  0.1684322  72.10384
# 7   91.41482  0.1778774  71.03413
# 9   90.65783  0.1834255  70.46279
# 11  90.19640  0.1873093  70.09004
# 13  89.88179  0.1909189  69.80177

### Accuracy on the training set is substantially worse than other models.
### So, I saw no need to proceed to the test set.