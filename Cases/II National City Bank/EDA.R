# LOTS of sausage-making going on here.

# Libs
library(dplyr)
library(vtreat)
library(caret)
library(ggplot2)

# Wd
setwd("~/Desktop/Andrew_Harvard_DataMining_Business/Cases/II National City Bank")

# Raw data
currentData   <- read.csv('training/CurrentCustomerMktgResults.csv')
vehData <- read.csv('training/householdVehicleData.csv') 
axiomData <- read.csv('training/householdAxiomData.csv')
creditData <- read.csv('training/householdCreditData.csv')

# Perform a join, need to add other data sets
joinData <- left_join(currentData, vehData, by = c('HHuniqueID'))
joinData <- left_join(joinData, axiomData, by = c('HHuniqueID'))
joinData <- left_join(joinData, creditData, by = c('HHuniqueID'))

# Baseline Acceptance Rate
totalAcceptedRate <- count(subset(joinData, joinData$Y_AcceptedOffer == 'Accepted')) / count(joinData)
print(totalAcceptedRate)

# subset acceptance rates
a <- data.frame(subset(joinData, joinData$past_Outcome == 'success'))

# past_Outcome = STRONG CORRELATION
aAcceptedRate <- count(subset(a, a$Y_AcceptedOffer == 'Accepted')) / count(a)
print(aAcceptedRate)

# luxury Car = no correlation
luxuryCars <- c('Audi', 'Porsche', 'Acura', 'Lexus', 'Lincoln', 'Lamborghini', 'BMW', 'Cadillac', 'Lotus', 'Infiniti', 'Bentley', 'Mercedes-Benz', 'Jaguar', 'Maserati', 'Volvo', 'Hummer', 'Aston Martin', 'Ferrari', 'Alfa Romeo', 'Land Rover', 'Maybach', 'Rolls-Royce', 'Shelby', 'McLaren', 'Tesla')
joinData$luxCar <- cbind(joinData$luxCar, ifelse(joinData$carMake %in% luxuryCars, 1, 0))
b <- subset(joinData, joinData$luxCar == 1)
bAcceptedRate <- count(subset(b, b$Y_AcceptedOffer == 'Accepted')) / count(b)
print(bAcceptedRate)
tail(b, 20)


# CarYr = no significant correlation
newCars <- c(2014, 2013, 2012, 2011, 2010)
joinData$newCar <- cbind(joinData$newCar, ifelse(joinData$carYr %in% newCars, 1, 0))

c <- subset(joinData, joinData$newCar == 1)
cAcceptedRate <- count(subset(c, c$Y_AcceptedOffer == 'Accepted')) / count(c)
print(cAcceptedRate)

unique(joinData$headOfhouseholdGender)

# Gender - also no significant correlation
d <- subset(joinData, joinData$headOfhouseholdGender == 'F')
dAcceptedRate <- count(subset(d, d$Y_AcceptedOffer == 'Accepted')) / count(d)
print(dAcceptedRate)
tail(d)

# annual donations - no significant correlation
e <- subset(joinData, nchar(joinData$annualDonations) > 0)
eAmt <- subset(joinData)
eAcceptedRate <- count(subset(e, e$Y_AcceptedOffer == 'Accepted')) / count(e)
print(eAcceptedRate)
eAcc <- subset(e, e$Y_AcceptedOffer == 'Accepted')
eAcc$annualDonations
e$annualDonations
typeof(joinData$annualDonations)
count(e)

# Affluence purchases - no significant correlation
f <- subset(joinData, joinData$AffluencePurchases == FALSE)
fAcceptanceRate <- count(subset(f, f$Y_AcceptedOffer == 'Accepted')) / count(f)
fAcceptanceRate
tail(f)

# Age also no significant correlation
mean(subset(joinData, joinData$Y_AcceptedOffer == 'Accepted')$Age)
mean(joinData$Age)

# Job - pretty strong correlation for students, retired, unemployed
# (negative correlation for blue-collar, services, housemaid, entrepreneur)
uniqueJobs <- unique(joinData$Job)
uniqueJobs <- uniqueJobs[!is.na(uniqueJobs)]

for (job in uniqueJobs) {
  g <- subset(joinData, joinData$Job == job)
  gAcceptanceRate <- count(subset(g, g$Y_AcceptedOffer == 'Accepted')) / count(g)
  print(job)
  print(gAcceptanceRate)
  print('------')
}

# Marital - 'single' slightly more likely to accept, 'married' slightly less likely
# Also redundancy with student???
h <- subset(joinData, joinData$Marital == 'divorced')
hAcceptanceRate <- count(subset(h, h$Y_AcceptedOffer == 'Accepted')) / count(h)
hAcceptanceRate

# education - tertiary = 46%, secondary = 37%, primary = 35%
i <- subset(joinData, joinData$Education == 'primary')
iAcceptanceRate <- count(subset(i, i$Y_AcceptedOffer == 'Accepted')) / count(i)
iAcceptanceRate

# DefaultOnRecord = 1 = 24%, 0 = 40%
j <- subset(joinData, joinData$DefaultOnRecord == 0)
jAcceptanceRate <- count(subset(j, j$Y_AcceptedOffer == 'Accepted')) / count(j)
jAcceptanceRate

# Recent Balance >1700 = 48%, <1700 = 38%
mean(joinData$RecentBalance)
mean(subset(joinData, joinData$Y_AcceptedOffer == 'Accepted')$RecentBalance)
k <- subset(joinData, joinData$RecentBalance < 1700)
kAcceptanceRate <- count(subset(k, k$Y_AcceptedOffer == 'Accepted')) / count(k)
kAcceptanceRate

# CarLoan - 1 = 29%, 0 = 42%
l <- subset(joinData, joinData$CarLoan == 0)
lAcceptanceRate <- count(subset(l, l$Y_AcceptedOffer == 'Accepted')) / count(l)
lAcceptanceRate