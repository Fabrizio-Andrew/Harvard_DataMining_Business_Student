setwd("~/Desktop/Andrew_Harvard_DataMining_Business/Cases/I Ok Cupid")

library(doBy)
library(maps)
library(ggplot2)
library(sf)

profilesData <- read.csv('profiles.csv')
addrData <- read.csv('addr.csv')
latlonData <- read.csv('LatLon.csv')

d <- data.frame(profilesData)
d2 <- data.frame(addrData)
d3 <- data.frame(latlonData)

# add state to profiles dataframe
loc <- match(d$location, d2$location)
d$state <- d2$state[loc]

# drop rows outside of California (most rows seem to be in CA)
d <- subset(d, state == 'California')

# create a variable to capture rows with unhealthy habits (obviously, some subjectivity involved here)
d$unhealthyHabits <- cbind(d$unhealthyHabits, ifelse(d$body_type %in% c('a little extra', 'curvy', 'full figured', 'overweight') |  
                                           d$drugs %in% c('sometimes', 'often') |
                                             d$drinks %in% c('often', 'very often', 'desperately') |
                                               d$smokes %in% c('sometimes', 'when drinking', 'yes', 'trying to quit'), 1, 0))
#
unhealthyPeople <- subset(d, unhealthyHabits == 1)
unhealthyLocations <- as.data.frame(table(unhealthyPeople['location']))
healthyPeople <- subset(d, unhealthyHabits == 0)
healthyLocations <- as.data.frame(table(unhealthyPeople['location']))


# add lat and lon to locations dataframes
loc2 <- match(unhealthyLocations$Var1, d3$location)
unhealthyLocations$lat <- d3$lat[loc2]
unhealthyLocations$lon <- d3$lon[loc2]

loc3 <- match(healthyLocations$Var1, d3$location)
healthyLocations$lat <- d3$lat[loc2]
healthyLocations$lon <- d3$lon[loc2]

# Calculate bubble sizes and alphas 
unhealthyLocations$size <- cbind(ifelse(unhealthyLocations$Freq > 500, 10, 
                                 ifelse(unhealthyLocations$Freq < 50, 1, unhealthyLocations$Freq/50)))
unhealthyLocations$alpha <- cbind(ifelse(unhealthyLocations$Freq > 300, .6, 
                                        ifelse(unhealthyLocations$Freq > 100, .3, .1)))

healthyLocations$size <- cbind(ifelse(healthyLocations$Freq > 500, 10, 
                                        ifelse(healthyLocations$Freq < 50, 1, healthyLocations$Freq/50)))
healthyLocations$alpha <- cbind(ifelse(healthyLocations$Freq > 300, .8, 
                                      ifelse(healthyLocations$Freq > 100, .5, .1)))

# plot bubble chart

states <- map_data('state')
CA <- subset(states, region == 'california')

p <- ggplot(data = CA) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = 'light green') +
  geom_point(data = healthyLocations, aes(x=lon, y=lat), colour = 'blue', size = healthyLocations$size, alpha = healthyLocations$alpha) +
  geom_point(data = unhealthyLocations, aes(x=lon, y=lat), colour = 'red', size = unhealthyLocations$size, alpha = unhealthyLocations$alpha) +
  coord_sf(xlim = c(-122.8, -121.8), ylim = c(37.2, 38.2))
p 


