## Applied Statistical Programing 
## Problem Set 7
## Gangyi Sun (441748)



library(dplyr)
library(ggplot2)



## 1) Import dataset 
setwd("~/Documents/GitHub/PS7")
crimeData <- read.csv("March2018.csv")



## 2) Compute number of crimes per day, by type of crime 
crimeData<-mutate(crimeData, Date=substr(DateOccur,1,10))   # obtains date of each crime
crimeData$Date<-as.Date(crimeData$Date, "%m/%d/%Y")
crimeData$March2018<-(format(crimeData$Date, "%Y-%m")=="2018-03")   # obtains month of each crime

# categorizes each type of crime (see pg 6 of FAQ in github for explanation of this method of categorization)
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

# counts number crimes per type per day, info contained in crimeDayType
crimeDayType <- crimeData %>% 
  group_by(Date, crimeType) %>% 
  summarise(count=n())
crimeDayType

# counts number of crimes per type for march, info contained in crimeTypeMarch 
crimeTypeMarch <- crimeData %>% 
  filter(March2018==TRUE) %>%
  group_by(crimeType) %>% 
  summarise(count=n())
id2<-which.max(crimeTypeMarch$count)
crimeTypeMarch$crimeType[id2]
crimeTypeMarch$count[id2]
# Larcency is the type of crime that happened the most (891) in March 2018.



## 3) Compute number of crimes per day, by neighborhood

# counts number crimes per neighborhood per day, info contained in crimeDayNeighborhood
crimeDayNeighborhood <- crimeData %>% 
  group_by(Date, Neighborhood) %>% 
  summarise(count=n())
crimeDayNeighborhood

# counts number of crimes per neighborhood for march, info contained in crimeNeighborhoodMarch 
crimeNeighborhoodMarch <- crimeData %>% 
  filter(March2018==TRUE) %>%
  group_by(Neighborhood) %>% 
  summarise(count=n())
id3<-which.max(crimeNeighborhoodMarch$count)
crimeNeighborhoodMarch$Neighborhood[id3]
crimeNeighborhoodMarch$count[id3]
# Neighborhood number 35 (Downtown) had the most number of crimes (294) in March 2018. 



## 4) Compute proportion of crime related to robbery, by district. 
unique(crimeData$District)
proporRobbery<-c()
# discard District==0 obs, since District unclear. 
for (i in 1:6){        
  oneDistrict<-filter(crimeData, District==i)     # loop through each district
  # counts number of crimes per type in district i 
  temp<-oneDistrict %>% 
    group_by(crimeType) %>% 
    summarise(count=n())
  sum(temp$count)     # total number of crimes in district i
  id<-which(temp$crimeType=="Robbery")      # obtains number of robberies in district i
  proportion<-temp$count[id]/sum(temp$count)      # proportion of total crimes in district i that are robberies 
  proporRobbery<-c(proporRobbery,proportion)      # creates vector of robbery proportion for districts 1 to 6
}
proporRobbery
which.max(proporRobbery)      # obtains district with the largest proportion of crime related to robbery 
# District 6 has the largest proport of crime related to robbery. 



## 5) Plot types of crime 

# Visualize changes of all types of crime over entire period of dates incldued in the datasaet 
crimeData %>% 
  group_by(Date) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=Date, y=count))+geom_line()+ggtitle("Number of crimes per day, full dataset")+ylab("Count")

# Visualize changes of all types of crime over March 2018 
crimeData %>% 
  filter(March2018==TRUE) %>%
  group_by(Date) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=Date, y=count))+geom_line()+ggtitle("Number of crimes per day, March 2018")+ylab("Count")



## 6) Plot types of crime by district 
crimeData$District<-as.factor(crimeData$District)

# Visualize changes of all types of crime over entire period of dates incldued in the datasaet, by district 
crimeData %>% 
  group_by(Date, District) %>% 
  summarise(count=n())%>%
  ggplot(aes(x=Date, y=count, group=District, color=District))+geom_line()+
  ggtitle("Number of crimes per day by district, full dataset")+ylab("Count")

# Visualize changes of all types of crime over March 2018, by district 
crimeData %>% 
  filter(March2018==TRUE) %>%
  group_by(Date, District) %>% 
  summarise(count=n())%>%
  ggplot(aes(x=Date, y=count, group=District, color=District))+geom_line(size=0.3)+
  ggtitle("Number of crimes per day by district, March 2018")+ylab("Count")

