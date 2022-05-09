setwd("~/Desktop/Andrew_Harvard_DataMining_Business/Cases/I Ok Cupid")

library(ggplot2)

profilesData <- read.csv('profiles.csv')

d <- data.frame(profilesData)

# Concatenate Sex and orientation to form a new variable
d$s <- paste0(d$sex, d$orientation)

# cast last_online value as date
d$last_online <- as.Date(d$last_online, format='%Y-%m-%d')

# 07/01/12 treated as "last date" since it's the most recent date in the data set
d$active <- cbind(d$active, ifelse(d$last_online >= as.Date('2012-07-01'), 4, 
                                   ifelse(d$last_online >= as.Date('2012-06-24'), 3, 
                                          ifelse(d$last_online >= as.Date('2012-06-01'), 2, 
                                                 ifelse(d$last_online >= as.Date('2012-04-01'), 1, 0)))))


# plot sex + sexuality against last_online
p <- ggplot(data=d, aes(x = factor(active), fill = s)) + 
  geom_bar(stat='count', position = 'dodge') +
  scale_x_discrete(breaks = c(0, 1, 2, 3, 4),
                   labels = c('year', '3 months', '1 month', '1 week', '1 day')) +
  ggtitle('Recent Activity vs. Gender and Sexuality') +
  xlab('Recent Activity') +
  theme_light()
p
