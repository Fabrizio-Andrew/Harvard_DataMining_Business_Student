setwd("~/Desktop/Andrew_Harvard_DataMining_Business/Cases/I Ok Cupid")

library(ggplot2)

profilesData <- read.csv('profiles.csv')

d <- data.frame(profilesData)

d2 <- subset(d, d$education != 'NA' & d$pets != 'NA')

class(d2$education)
unique(d2$pets)
quitConditions <- c('dropped out of space camp', 'dropped out of college/university', 'dropped out of ph.d program', 'dropped out of med school', 'dropped out of law school', 'dropped out of high school', 'dropped out of two-year college', 'dropped out of masters program')
petConditions <- c('has dogs', 'has dogs and has cats', 'has dogs and dislikes cats', 'has cats', 'has dogs and likes cats')

d2$quit <- ifelse(d2$education %in% quitConditions, 1, 0)
d2$p <- ifelse(d2$pets %in% petConditions, 1, 0)


quitPetsPct <- nrow(subset(d2, d2$quit == 1 & d2$p == 1)) / nrow(subset(d2, d2$quit == 1)) * 100
noquitPetsPct <- nrow(subset(d2, d2$quit == 0 & d2$p == 1)) / nrow(subset(d2, d2$quit == 0)) * 100

Pcts <- c(quitPetsPct, noquitPetsPct)
quitters <- c(1, 0)

# construct datafrome with categories and pcts
dfPcts <- data.frame(quitters, Pcts)

# plot quitters vs. pet ownership pcts
p <- ggplot(data=dfPcts, aes(x = factor(quitters), y = Pcts)) + 
  geom_bar(stat = 'identity') +
  scale_x_discrete(breaks = c(0, 1),
                   labels = c('Dropouts', 'Non-Dropouts')) +
  ggtitle('Dropouts (any level) vs. Pet ownership') +
  xlab('Education') +
  ylab('Percent Owning Pets') +
  theme_light()
p
