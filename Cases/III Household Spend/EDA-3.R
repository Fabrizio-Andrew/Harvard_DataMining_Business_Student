# mmmmmmmmmmm... Sausage!

# Libs
library(dplyr)
library(vtreat)
library(caret)
library(ggplot2)
library(tidyr)

# Wd
setwd("~/Desktop/Andrew_Harvard_DataMining_Business/Cases/III Household Spend")

# Raw Data
consumerData   <- read.csv('studentTables/training/consumerData_training15K_studentVersion.csv')
donationsData   <- read.csv('studentTables/training/DonationsData_training15K_studentVersion.csv')
magazineData   <- read.csv('studentTables/training/magazineData_training15K_studentVersion.csv')
politicalData   <- read.csv('studentTables/training/politicalData_training15K_studentVersion.csv')
inHouseData   <- read.csv('studentTables/training/inHouseData_training15K_studentVersion.csv')

# Joins
joinData <- left_join(inHouseData, donationsData, by = c('tmpID'))
joinData <- left_join(joinData, magazineData, by = c('tmpID'))
joinData <- left_join(joinData, politicalData, by = c('tmpID'))
joinData <- left_join(joinData, consumerData, by = c('tmpID'))

sortedData <- joinData[order(joinData$yHat), decreasing = TRUE]
tail(sortedData, 20)

# Let's start plotting some ideas
# Age - not promising
ggplot(joinData, aes(x=Age, y=yHat)) + 
  geom_point(size=.05) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)



# State - some correlation
a <- c()
b <- c()
states <- as.vector(unique(joinData$state))
for (s in states) {
  stateRows <- joinData[joinData$state == s,]
  a <- append(a, s)
  b <- append(b, mean(stateRows$yHat))
}

df <- data.frame(state=a, meanyHat=b)
ggplot(df, aes(x=a, y=b)) + 
  geom_point(size=1)


# Home Purchase Price - no correlation
ggplot(joinData, aes(x=HomePurchasePrice, y=yHat)) + 
  geom_point(size=.05) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Land Value - no correlation
ggplot(joinData, aes(x=LandValue, y=yHat)) + 
  geom_point(size=.05) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Dwelling Unit Size - moderate correlation
a <- c()
b <- c()
unitSizes <- as.vector(unique(joinData$DwellingUnitSize))
for (s in unitSizes) {
  dwellingRows <- joinData[joinData$DwellingUnitSize == s,]
  a <- append(a, s)
  b <- append(b, mean(dwellingRows$yHat))
}

df <- data.frame(unitSizes=a, meanyHat=b)
ggplot(df, aes(x=a, y=b)) + 
  geom_point(size=1)

# storeVisitFrequency - no correlation
a <- c()
b <- c()
visits <- as.vector(unique(joinData$storeVisitFrequency))
for (s in visits) {
  visits <- joinData[joinData$storeVisitFrequency == s,]
  a <- append(a, s)
  b <- append(b, mean(visits$yHat))
}

df <- data.frame(visitFrequency=a, meanyHat=b)
ggplot(df, aes(x=a, y=b)) + 
  geom_point(size=1)

# PropertyType - moderate correlation
a <- c()
b <- c()
propTypes <- as.vector(unique(joinData$PropertyType))
for (s in propTypes) {
  types <- joinData[joinData$PropertyType == s,]
  a <- append(a, s)
  b <- append(b, mean(types$yHat))
}

df <- data.frame(PropertyTypes=a, meanyHat=b)
ggplot(df, aes(x=a, y=b)) + 
  geom_point(size=1)

# Donations - strong correlation w/ donation score

joinData$score1 <- ifelse(joinData$DonatesToCharityInHome == "Yes", 1, 0)
joinData$score2 <- ifelse(joinData$DonatestoAnimalWelfare == "Yes", 1, 0)
joinData$score3 <- ifelse(joinData$DonatestoChildrensCauses == "Yes", 1, 0)
joinData$score4 <- ifelse(joinData$DonatestoHealthcare == "Yes", 1, 0)
joinData$score5 <- ifelse(joinData$DonatestoInternationalAidCauses == "Yes", 1, 0)
joinData$score6 <- ifelse(joinData$DonatestoVeteransCauses == "Yes", 1, 0)
joinData$score7 <- ifelse(joinData$DonatestoArtsandCulture == "Yes", 1, 0)
joinData$score8 <- ifelse(joinData$DonatestoWildlifePreservation == "Yes", 1, 0)
joinData$score9 <- ifelse(joinData$DonatestoLocalCommunity == "Yes", 1, 0)

joinData$donationScore <- joinData$score1 + 
                          joinData$score2 +
                          joinData$score3 +
                          joinData$score4 +
                          joinData$score5 +
                          joinData$score6 +
                          joinData$score7 +
                          joinData$score8 +
                          joinData$score9
unique(joinData$donationScore)

uniqueScores <- sort(unique(joinData$donationScore))
a <- c()
b <- c()
for (s in uniqueScores) {
  scores <- joinData[joinData$donationScore == s,]
  a <- append(a, s)
  b <- append(b, mean(scores$yHat))
}

df <- data.frame(donationScore=a, meanyHat=b)
ggplot(df, aes(x=a, y=b)) + 
  geom_point(size=1)


# Magazines - surprisingly strong correlation here

mags <- c("FamilyMagazineInHome", "FemaleOrientedMagazineInHome", "GardeningMagazineInHome", "CulinaryInterestMagazineInHome", "DoItYourselfMagazineInHome")
b <- c()
# Loop wasn't working here and I don't want to spend the time to sort it out
magRows <- joinData[joinData$FamilyMagazineInHome != "",]
b <- append(b, mean(as.integer(magRows$yHat)))

magRows <- joinData[joinData$FemaleOrientedMagazineInHome != "",]
b <- append(b, mean(as.integer(magRows$yHat)))

magRows <- joinData[joinData$GardeningMagazineInHome != "",]
b <- append(b, mean(as.integer(magRows$yHat)))

magRows <- joinData[joinData$CulinaryInterestMagazineInHome != "",]
b <- append(b, mean(as.integer(magRows$yHat)))

magRows <- joinData[joinData$DoItYourselfMagazineInHome != "",]
b <- append(b, mean(as.integer(magRows$yHat)))

df <- data.frame(magazines=mags, meanyHat=b)
ggplot(df, aes(x=magazines, y=meanyHat)) + 
  geom_point(size=1)

# PresenceOfChildrenCode - weak correlation (surprising)
a <- c()
b <- c()
childCodes <- as.vector(unique(joinData$PresenceOfChildrenCode))
for (s in childCodes) {
  rows <- joinData[joinData$PresenceOfChildrenCode == s,]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(ChildCode=a, meanyHat=b)
ggplot(df, aes(x=ChildCode, y=meanyHat)) + 
  geom_point(size=1)

# HomeOwnerRenter - no correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$HomeOwnerRenter))
for (s in uniques) {
  rows <- joinData[joinData$HomeOwnerRenter == s,]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(OwnerRenter=a, meanyHat=b)
ggplot(df, aes(x=OwnerRenter, y=meanyHat)) + 
  geom_point(size=1)

# MedianEducationYears - weak correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$MedianEducationYears))
for (s in uniques) {
  rows <- joinData[joinData$MedianEducationYears == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(EducationYears=a, meanyHat=b)
ggplot(df, aes(x=EducationYears, y=meanyHat)) + 
  geom_point(size=1)

# NetWorth - weak correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$NetWorth))
for (s in uniques) {
  rows <- joinData[joinData$NetWorth == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(NetWorth=a, meanyHat=b)
ggplot(df, aes(x=NetWorth, y=meanyHat)) + 
  geom_point(size=1)

# MedianEducationYears - no correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$Investor))
for (s in uniques) {
  rows <- joinData[joinData$Investor == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(Investor=a, meanyHat=b)
ggplot(df, aes(x=Investor, y=meanyHat)) + 
  geom_point(size=1)

# BusinessOwner - no correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$BusinessOwner))
for (s in uniques) {
  rows <- joinData[joinData$BusinessOwner == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(BusinessOwner=a, meanyHat=b)
ggplot(df, aes(x=BusinessOwner, y=meanyHat)) + 
  geom_point(size=1)

# OccupationIndustry - moderate correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$OccupationIndustry))
for (s in uniques) {
  rows <- joinData[joinData$OccupationIndustry == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(OccupationIndustry=a, meanyHat=b)
ggplot(df, aes(x=OccupationIndustry, y=meanyHat)) + 
  geom_point(size=1)

# UpscaleBuyerInHome - strong (negative) correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$UpscaleBuyerInHome))
for (s in uniques) {
  rows <- joinData[joinData$UpscaleBuyerInHome == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(UpscaleBuyer=a, meanyHat=b)
ggplot(df, aes(x=UpscaleBuyer, y=meanyHat)) + 
  geom_point(size=1)

# BuyerOfAntiques - Only 1 antique buyer in the whole dataset!
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$BuyerofAntiquesinHousehold))
for (s in uniques) {
  rows <- joinData[joinData$BuyerofAntiquesinHousehold == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(AntiqueBuyer=a, meanyHat=b)
ggplot(df, aes(x=AntiqueBuyer, y=meanyHat)) + 
  geom_point(size=1)

# BuyerofArtinHousehold - no correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$BuyerofArtinHousehold))
for (s in uniques) {
  rows <- joinData[joinData$BuyerofArtinHousehold == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(ArtBuyer=a, meanyHat=b)
ggplot(df, aes(x=ArtBuyer, y=meanyHat)) + 
  geom_point(size=1)

# GeneralCollectorinHousehold - no correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$GeneralCollectorinHousehold))
for (s in uniques) {
  rows <- joinData[joinData$GeneralCollectorinHousehold == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(Collector=a, meanyHat=b)
ggplot(df, aes(x=Collector, y=meanyHat)) + 
  geom_point(size=1)

# ComputerOwnerInHome - moderate correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$ComputerOwnerInHome))
for (s in uniques) {
  rows <- joinData[joinData$ComputerOwnerInHome == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(ComputerOwnerInHome=a, meanyHat=b)
ggplot(df, aes(x=ComputerOwnerInHome, y=meanyHat)) + 
  geom_point(size=1)

# Gender - very weak correlation
a <- c()
b <- c()
uniques <- as.vector(unique(joinData$Gender))
for (s in uniques) {
  rows <- joinData[joinData$Gender == s,]
  rows <- rows[!is.na(rows$yHat),]
  a <- append(a, s)
  b <- append(b, mean(rows$yHat))
}

df <- data.frame(Gender=a, meanyHat=b)
ggplot(df, aes(x=Gender, y=meanyHat)) + 
  geom_point(size=1)