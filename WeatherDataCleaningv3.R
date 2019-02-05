##Weather data Cleaning V3
#Drug Smuggling at Terrorism Project

#load libraries
library(VFS)
library(rnoaa)
library(tidyverse)
library(magrittr)

##Get names of Stations
#Each File has all data for a single wheather station
stationfiles<-list.files("./ghcnd_all/") #Get a list of filenames in the folder

stationfile<-stationfiles[1]
sname<- str_extract(stationfile,"\\D+")  %>% str_remove("_") 

## Function For loading and Subsetting data
cleandat<-function(stationfile) 
d<-read.dly(paste("./ghcnd_all/",stationfile,sep=""))%>%
  subset(select=c("YEAR","MONTH","DAY","PRCP.VALUE","TMAX.VALUE","TMIN.VALUE"))%>%
  na.omit()%>%
  mutate(TAVG= (TMIN.VALUE+TMAX.VALUE)/2)%>%
  subset(YEAR>=1970, YEAR<=2014)%>%
  mutate(fips= substr(sname, 1, 2))
####I did not aggregate data in this step because many countries have several stations

print(cleandat(stationfile))
mydata<-lapply(stationfiles[1:length(stationfiles)], cleandat)


##Code past this point has not been tested on the full file
mydata<-do.call(rbind, lapply(mydata, data.frame, stringsAsFactors=FALSE))
mydata

opium_ht<-22
opium_lt<-15

mj_ht<-27
mj_lt<-6

coca_ht<-25
coca_lt<-18

  
mydata%>%group_by(fips, YEAR, MONTH, DAY) %>% 
  summarise_each(funs(mean))%>%
  mutate(TAVG= (TMIN.VALUE+TMAX.VALUE)/2)%>%
  group_by(fips=fips, month=MONTH, year=YEAR ) %>% 
  summarise(ave_mo_t=mean(TAVG ,na.rm=TRUE), opium_t=sum(as.numeric(opium_lt <= TAVG &  TAVG<=opium_ht)),mj_t=sum(as.numeric(mj_lt <= TAVG &      TAVG<=mj_ht)),coca_t=sum(as.numeric(coca_lt <= TAVG &  TAVG<=coca_ht)) ) 
              