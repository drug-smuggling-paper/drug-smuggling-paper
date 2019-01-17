##Weather Data Cleaning

#load data
library(readr)
Afghanistan_1974 <- read_csv("temperature/Afghanistan_1974.csv")

##
Afghanistan_1974$TAVG[Afghanistan_1974$TAVG==-9999]<-NA
Afghanistan_1974<-aggregate(TAVG~DATE, data=Afghanistan_1974, FUN= mean)
Afghanistan_1974$month<-substr(Afghanistan_1974$DATE, 5, 6)

##Opium
Opium<-Afghanistan_1974
Opium$drug<-rep("Opium")
Opium$htemp<-rep(74.3)
Opium$ltemp<-rep(42.08)

Opium$idealtemp
Opium$idealtemp[Opium$ltemp <= Opium$TAVG & Opium$TAVG <= Opium$htemp]<-1
Opium$idealtemp[is.na(Opium$idealtemp)]<-0

Opium<-aggregate(idealtemp~month, data= Opium, FUN= sum)

Opium$country<-rep("Afghanistan")
Opium$year<-rep(1974)
Opium$drug<-rep("Opium")
