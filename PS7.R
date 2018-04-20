## Applied Statistical Programing 
## Problem Set 7
## Gangyi Sun (441748)


library(dplyr)
library(ggplot2)


## 1) Import dataset 
setwd("~/Documents/GitHub/PS7")
crimeData <- read.csv("March2018.csv")


## 2) Compute number of crimes per day, by type of crime 
crimeData<-mutate(crimeData, Date=substr(DateOccur,1,10))
crimeData$Date<-as.Date(crimeData$Date, "%m/%d/%Y")
crimeData$March2018<-(format(crimeData$Date, "%Y-%m")=="2018-03")

unique(crimeData$Crime)
crimeData$crimeType<-""
for (i in 1:nrow(crimeData)){
  if (crimeData$Crime[i]==10000){
    crimeData$crimeType[i]<-"Homicide"
  } else if (crimeData$Crime[i]>=20000 & crimeData$Crime[i]<30000) {
    crimeData$crimeType[i]<-"Rape"
  } else if (crimeData$Crime[i]>=30000 & crimeData$Crime[i]<40000) {
    crimeData$crimeType[i]<-"Robbery"
  } else if (crimeData$Crime[i]>=40000 & crimeData$Crime[i]<50000) {
    crimeData$crimeType[i]<-"Aggravated Assault"
  } else if (crimeData$Crime[i]>=50000 & crimeData$Crime[i]<60000) {
    crimeData$crimeType[i]<-"Robbery"
  } else if (crimeData$Crime[i]>=60000 & crimeData$Crime[i]<70000) {
    crimeData$crimeType[i]<-"Larcency"
  } else if (crimeData$Crime[i]>=70000 & crimeData$Crime[i]<80000) {
    crimeData$crimeType[i]<-"Vehicle Theft"
  } else if (crimeData$Crime[i]>=80000 & crimeData$Crime[i]<90000) {
    crimeData$crimeType[i]<-"Arson"
  } else if (crimeData$Crime[i]>=90000 & crimeData$Crime[i]<100000) {
    crimeData$crimeType[i]<-"Simple Assault"
  } else if (crimeData$Crime[i]>=100000 & crimeData$Crime[i]<110000) {
    crimeData$crimeType[i]<-"Forgery"
  } else if (crimeData$Crime[i]>=110000 & crimeData$Crime[i]<120000) {
    crimeData$crimeType[i]<-"Fraud"
  } else if (crimeData$Crime[i]>=120000 & crimeData$Crime[i]<130000) {
    crimeData$crimeType[i]<-"Embezzlement"
  } else if (crimeData$Crime[i]>=130000 & crimeData$Crime[i]<140000) {
    crimeData$crimeType[i]<-"Stolen Property"
  } else if (crimeData$Crime[i]>=140000 & crimeData$Crime[i]<150000) {
    crimeData$crimeType[i]<-"Destruction of Property"
  } else if (crimeData$Crime[i]>=150000 & crimeData$Crime[i]<160000) {
    crimeData$crimeType[i]<-"Weapons Related"
  } else if (crimeData$Crime[i]>=170000 & crimeData$Crime[i]<180000) {
    crimeData$crimeType[i]<-"Sex Related Offence"
  } else if (crimeData$Crime[i]>=180000 & crimeData$Crime[i]<190000) {
    crimeData$crimeType[i]<-"Drug Possession/Sale"
  } else if (crimeData$Crime[i]>=200000 & crimeData$Crime[i]<210000) {
    crimeData$crimeType[i]<-"Family & Child Crime"
  } else if (crimeData$Crime[i]>=210000 & crimeData$Crime[i]<220000) {
    crimeData$crimeType[i]<-"DUI"
  } else if (crimeData$Crime[i]>=220000 & crimeData$Crime[i]<230000) {
    crimeData$crimeType[i]<-"Liquor Related"
  } else if (crimeData$Crime[i]>=240000 & crimeData$Crime[i]<250000) {
    crimeData$crimeType[i]<-"Disorderly Conduct"
  } else if (crimeData$Crime[i]>=250000 & crimeData$Crime[i]<260000) {
    crimeData$crimeType[i]<-"Loitering"
  } else if (crimeData$Crime[i]>=260000) {
    crimeData$crimeType[i]<-"All Others"
  }
}

crimeDayType <- crimeData %>% 
  group_by(Date, crimeType) %>% 
  summarise(count=n())
crimeDayType

crimeTypeMarch <- crimeData %>% 
  filter(March2018==TRUE) %>%
  group_by(crimeType) %>% 
  summarise(count=n())
id2<-which.max(crimeTypeMarch$count)
crimeTypeMarch$crimeType[id2]
crimeTypeMarch$count[id2]
# Larcency is the type of crime that happened the most (891) in March 2018.


## 3) Compute number of crimes per day, by neighborhood
crimeDayNeighborhood <- crimeData %>% 
  group_by(Date, Neighborhood) %>% 
  summarise(count=n())
crimeDayNeighborhood

crimeNeighborhoodMarch <- crimeData %>% 
  filter(March2018==TRUE) %>%
  group_by(Neighborhood) %>% 
  summarise(count=n())
id3<-which.max(crimeNeighborhoodMarch$count)
crimeNeighborhoodMarch$Neighborhood[id3]
crimeNeighborhoodMarch$count[id3]
# Neighborhood number 35 (Downtown) had the most number of crimes per day (294) in March 2018. 


## 4) Compute proportion of crime related to robbery, by district. 
unique(crimeData$District)
proporRobbery<-c()
# discard District==0 obs, since District unclear. 
for (i in 1:6){        
  oneDistrict<-filter(crimeData, District==i)
  temp<-oneDistrict %>% 
    group_by(crimeType) %>% 
    summarise(count=n())
  sum(temp$count)
  id<-which(temp$crimeType=="Robbery")
  proportion<-temp$count[id]/sum(temp$count)
  proporRobbery<-c(proporRobbery,proportion)
}
proporRobbery
which.max(proporRobbery)
# District 6 has the largest proport of crime related to robbery. 


## 5) Plot types of crime 
ggplot(data=crimeData)+geom_bar(mapping = aes(x = crimeType))


## 6) Plot types of crime by district 
ggplot(data=crimeData)+geom_bar(mapping = aes(x = crimeType))
