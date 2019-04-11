##Weather data Cleaning V3
#Drug Smuggling at Terrorism Project

#load libraries
#library(VFS)
#library(rnoaa)
#library(tidyverse)
#library(magrittr)
library(parallel)

mk_worker<-function(countrylist, stationfiles) {
  force(stationfiles)
  
  cleanonecounry<-function(onecountryfiles) {
    
    opium_ht<-23.5
    opium_lt<-5.6
    mj_ht<-27
    mj_lt<-6
    coca_ht<-25
    coca_lt<-18
    d<-read.dly(paste("../ghcnd_all/",onecountryfiles,sep=""))   %>%
      filter(YEAR>=1970) %>%
      subset(select=c("YEAR","MONTH","DAY","PRCP.VALUE","TMAX.VALUE","TMIN.VALUE"))%>%
      mutate(TAVG= (TMIN.VALUE+TMAX.VALUE)/2, opium_t=as.numeric(opium_lt <= TAVG &  TAVG <= opium_ht),mj_t=as.numeric(mj_lt <= TAVG & TAVG<=mj_ht),
             coca_t=as.numeric(coca_lt <= TAVG &  TAVG<=coca_ht)) %>%
      group_by(MONTH, YEAR) %>%
      summarise(ave_precip_mo=mean(PRCP.VALUE,na.rm=TRUE),ave_mo_t=mean(TAVG ,na.rm=TRUE), opium_t=sum(opium_t,na.rm=TRUE),mj_t=sum(mj_t,na.rm=TRUE),coca_t=sum(coca_t,na.rm=TRUE) ) %>% 
      ungroup %>%
      mutate(fips= substr(onecountryfiles,1,2), year=YEAR, station=onecountryfiles)
    return(d)
  }
  
  #countrylist="AG"
  #onecountryfiles<-stationfiles[countrylist==substr(stationfiles,1, 2)] #select all files for the first country
  #d<-do.call(rbind, lapply(onecountryfiles, cleanonecounry))
  
  
  wk_run<-function(countrylist=countrylist){
    onecountryfiles<-stationfiles[countrylist==substr(stationfiles,1, 2)] #select all files for the first country
    d<-do.call(rbind, lapply(onecountryfiles, cleanonecounry))  
    d <-d %>% 
      group_by(MONTH, YEAR) %>% 
      summarise(ave_precip_mo=mean(ave_precip_mo, na.rm=TRUE), ave_mo_t=mean(ave_mo_t,na.rm=TRUE), opium_t=mean(opium_t,na.rm=TRUE),mj_t=mean(mj_t,na.rm=TRUE), coca_t=mean(coca_t,na.rm=TRUE), fips=first(fips)) %>%
      ungroup   
    write.table(d, paste0(countrylist,".txt"), row.names=FALSE)
    return(d)
  }
  
  #wk_run(countrylist)
  #lapply(countrylist[1],wk_run)
  
  return(wk_run)
}

#mk_worker(stationfiles=stationfiles, countrylist=countrylist[1])

stationfiles<-list.files("../ghcnd_all/") #Get a list of filenames in the folder
countrylist<-substr(stationfiles,1, 2) #get a list of country identifiers
countrylist<-unique(countrylist) #get a vector of unique identifiers for each country

myt<-proc.time()[3] #Start the timer
parallelCluster <- parallel::makeCluster(20)

clusterEvalQ(parallelCluster, {
  library(magrittr)
  library(VFS)
  library(tidyverse)
})
parallel::parLapplyLB(parallelCluster,countrylist, mk_worker(stationfiles=stationfiles))
#Stop cluster:
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- c()
}
(proc.time()[3]-myt)/60 #Check the timer


