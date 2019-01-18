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
Opium$Drug<-rep("Opium")

Opium$htemp<-rep(74.3)
Opium$ltemp<-rep(42.08)

Opium$idealtemp
Opium$idealtemp[Opium$ltemp <= Opium$TAVG & Opium$TAVG <= Opium$htemp]<-1
Opium$idealtemp[is.na(Opium$idealtemp)]<-0

Opium<-aggregate(idealtemp~month, data= Opium, FUN= sum)
Opium$drug<-rep("Opium")

#Marijuana
Marijuana<-Afghanistan_1974
Marijuana$Drug<-rep("Marijuana")
Marijuana$htemp<-rep(80.6)
Marijuana$ltemp<-rep(42.8)

Marijuana$idealtemp
Marijuana$idealtemp[Marijuana$ltemp <= Marijuana$TAVG & Marijuana$TAVG <= Marijuana$htemp]<-1
Marijuana$idealtemp[is.na(Marijuana$idealtemp)]<-0

Marijuana<-aggregate(idealtemp~month, data= Marijuana, FUN= sum)
Marijuana$drug<-rep("Marijuana")

##Cocaine
Cocaine<-Afghanistan_1974
Cocaine$Drug<-rep("Cocaine")
Cocaine$htemp<-rep(77)
Cocaine$ltemp<-rep(64.4)

Cocaine$idealtemp
Cocaine$idealtemp[Cocaine$ltemp <= Cocaine$TAVG & Cocaine$TAVG <= Cocaine$htemp]<-1
Cocaine$idealtemp[is.na(Cocaine$idealtemp)]<-0

Cocaine<-aggregate(idealtemp~month, data= Cocaine, FUN= sum)
Cocaine$drug<-rep("Cocaine")

##Put it back together
Afghanistan_1974<-rbind(Opium, Marijuana, Cocaine)
Afghanistan_1974$country<-rep("Afghanistan")
Afghanistan_1974$year<-rep(1974)

