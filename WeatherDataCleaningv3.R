##Weather data Cleaning V3
#Drug Smuggling at Terrorism Project

#load libraries
library(VFS)
library(rnoaa)
library(tidyverse)
library(magrittr)
library(parallel)

##Get names of Stations
#Each File has all data for a single wheather station

stationfiles<-list.files("./ghcnd_all/")

stationfile<-stationfiles[1]
sname<- str_extract(stationfile,"\\D+")  %>% str_remove("_") 

## Function For loading and Subsetting data


 
  
mk_worker<-function(countrylist, stationfiles) {
    force(stationfiles)
    
  
cleanonecounry<-function(onecountryfiles) {
  opium_ht<-74.3
  opium_lt<-42.08
  mj_ht<-80.6
  mj_lt<-42.8
  coca_ht<-77
  coca_lt<-64.4
  
  
  d<-read.dly(paste("./ghcnd_all/",onecountryfiles,sep="")) %>%
  mutate(isday=rep(1))%>%
  subset(select=c("YEAR","MONTH","DAY","PRCP.VALUE","TMAX.VALUE","TMIN.VALUE","isday"))%>%
  na.omit()%>%
  mutate(TAVG= (TMIN.VALUE+TMAX.VALUE)/2)%>%
  group_by(MONTH, YEAR)%>%
  summarise(ave_mo_t=mean(TAVG ,na.rm=TRUE), opium_t=sum(as.numeric(opium_lt <= TAVG &  TAVG<=opium_ht)),mj_t=sum(as.numeric(mj_lt <= TAVG & TAVG<=mj_ht)),
            coca_t=sum(as.numeric(coca_lt <= TAVG &  TAVG<=coca_ht)), days=sum(as.numeric(isday)) )%>%
  subset(days>28)%>%
  ungroup %>%
  mutate(fips= substr(onecountryfiles,1,2), year=YEAR)%>%
  return(d)
}
####I did not aggregate data in this step because many countries have several stations

#country=countrylist[1:2]
#onecountryfiles<-stationfiles[countrylist==substr(stationfiles,1, 2)] #select all files for the first country

#wk_run does one country from countrylist
wk_run<-function(countrylist=countrylist){
  onecountryfiles<-stationfiles[countrylist==substr(stationfiles,1, 2)] #select all files for the first country
  d<-do.call(rbind, lapply(onecountryfiles, cleanonecounry))
  return(d)
}

#wk_run(countrylist)
#lapply(countrylist[1:2],wk_run)

return(wk_run)
}





mk_worker(stationfiles=stationfiles, countrylist=countrylist[1])

stationfiles<-list.files("./ghcnd_all/") #Get a list of filenames in the folder
countrylist<-substr(stationfiles,1, 2) #get a list of country identifiers
countrylist<-unique(countrylist) #get a vector of unique identifiers for each country


myt<-proc.time()[3] #Start the timer
parallelCluster <- parallel::makeCluster(3)

clusterEvalQ(parallelCluster, {
  library(magrittr)
  library(VFS)
  library(tidyverse)
})
mydata<-parallel::parLapplyLB(parallelCluster,countrylist, mk_worker(stationfiles=stationfiles))
#Stop cluster:
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- c()
}
(proc.time()[3]-myt)/60 #Check the timer




myt<-proc.time()[3] #Start the timer
mydata<-mclapply(countrylist[1:3], cleandat)
(proc.time()[3]-myt)/60 #Check the timer

mydata<-do.call(rbind, mydata)


write.csv(mydata, "weatherdata.csv", row.names = FALSE)
