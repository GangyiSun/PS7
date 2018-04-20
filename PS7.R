## Applied Statistical Programing 
## Problem Set 7
## Gangyi Sun (441748)


library(dplyr)
library(ggplot2)


## 1) Import dataset 
setwd("~/Documents/GitHub/PS7")
crimeData <- read.csv("March2018.csv")


## 2) Compute number of crimes per day, by type of crime 
unique(crimeData$Description)


## 3) Compute number of crimes per day, by neighborhood
numCrimeNeighborhood <- crimeData %>% 
  group_by(Neighborhood) %>% 
  summarise(count=n())
numCrimeNeighborhood<-mutate(numCrimeNeighborhood, numCrimePerDay=(count/31))
id3<-which.max(numCrimeNeighborhood$numCrimePerDay)
numCrimeNeighborhood$Neighborhood[id3]
numCrimeNeighborhood$numCrimePerDay[id3]
# Neighborhood number 35 (Downtown) had the most number of crimes per day (9.8) in March 2018. 



