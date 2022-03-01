setwd("~/Desktop/Andrew_Harvard_DataMining_Business/Cases/I Ok Cupid")

library(ggplot2)

profilesData <- read.csv('profiles.csv')

d <- data.frame(profilesData)

#unique(d$diet)
#unique(d$sign)

# create variable to categorize all vegetarians/vegans
d$veg <- cbind(d$veg, ifelse(d$diet %in% c('mostly vegetarian', 'mostly vegan', 'strictly vegan', 'strictly vegetarian', 'vegan', 'vegetarian'), 1, 0))

# create variables for levels that astrology matters
d$astrology <- cbind(d$astrology, ifelse(grepl('it matters a lot', d$sign, fixed=TRUE), 2,
                     ifelse(d$sign == 'NA', 0,
                            ifelse(grepl("it doesn't matter", d$sign, fixed=TRUE), 0, 1))))

# Calculate percentage of vegans/vegetarians by category
# total
totVegPct <- nrow(subset(d, d$veg == 1)) / nrow(d) * 100

# astroLow
fAstroLowVegPct <- nrow(subset(d, d$veg == 1 & d$astrology == 0 & d$sex == 'f')) / nrow(subset(d, d$astrology == 0 & d$sex == 'f')) * 100
mAstroLowVegPct <- nrow(subset(d, d$veg == 1 & d$astrology == 0 & d$sex == 'm')) / nrow(subset(d, d$astrology == 0 & d$sex == 'm')) * 100

# astroMed
fAstroMedVegPct <- nrow(subset(d, d$veg == 1 & d$astrology == 1 & d$sex == 'f')) / nrow(subset(d, d$astrology == 1 & d$sex == 'f')) * 100
mAstroMedVegPct <- nrow(subset(d, d$veg == 1 & d$astrology == 1 & d$sex == 'm')) / nrow(subset(d, d$astrology == 1 & d$sex == 'm')) * 100

# astroHigh
fAstroHiVegPct <- nrow(subset(d, d$veg == 1 & d$astrology == 2 & d$sex == 'f')) / nrow(subset(d, d$astrology == 2 & d$sex == 'f')) * 100
mAstroHiVegPct <- nrow(subset(d, d$veg == 1 & d$astrology == 2 & d$sex == 'm')) / nrow(subset(d, d$astrology == 2 & d$sex == 'm')) * 100

astros <- c(0, 0, 1, 1, 2, 2)
s <- c('M', 'F', 'M', 'F', 'M', 'F')
Pcts <- c(mAstroLowVegPct, fAstroLowVegPct, mAstroMedVegPct, fAstroMedVegPct, mAstroHiVegPct, fAstroHiVegPct)
print(Pcts)

# construct datafrome with categories and pcts
dfPcts <- data.frame(astros, s, Pcts)

# plot vegetarian/vegan against astrology levels
p <- ggplot(data=dfPcts, aes(x = factor(astros), y = Pcts, fill = s)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_x_discrete(breaks = c(0, 1, 2),
                   labels = c('Astr Low Imp', 'Astr Med Imp', 'Astr High Imp')) +
  ggtitle('Astrology Importance vs. Vegan/Vegetarian Rate') +
  xlab('Astrology Importance') +
  ylab('Percent Vegan/Vegetarian') +
  theme_light()
p

