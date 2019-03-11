##Drug Smuggling and Terrorism

#Load data
library(data.table)
library(readr)
library(tidyr)
library(dplyr)
library(countrycode)
drugdata<-read.csv("drugdata.csv")
gtd<-fread("globalterrorismdb.csv")

#aggreagte gtd and make measure for number of attacks and number of sophisiticated attacks
gtd$sophisticated[gtd$attacktype1==4]<-1
gtd$sophisticated[gtd$attacktype1==1]<-1
gtd$sophisticated[is.na(gtd$sophisticated)]<-0
gtdgroup<-aggregate(eventid~gname+iyear+imonth, data=gtd, FUN=length)
gtdsoph<-aggregate(sophisticated~gname+iyear+imonth, data=gtd, FUN=sum)

gtdgroup<-merge(gtdgroup, gtdsoph, by=c("iyear","imonth","gname"))

##Funtioun to get modal value
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


targetcountry<-aggregate(country_txt~gname, data=gtd, FUN= Mode)%>%rename(targetcountry=country_txt)

targetcountry$targetccode<-countrycode(targetcountry$targetcountry,"country.name","cown")

gtdgroup<-merge(gtdgroup,targetcountry, by="gname")

#Prep and merge Drug data
drugdata$narcotics<-1
gtddrug<-merge(gtdgroup,drugdata, by.x="gname", by.y="GTD.name",all = TRUE)


gtddrug$narcotics[is.na(gtddrug$narcotics)]<-0

#subset
gtddrug2<-subset(gtddrug, select=c("gname","iyear","imonth","eventid","Drug1","sophisticated",
                                   "Plant1","Plant2","Harvest1","Harvest2","Harvest3","narcotics","targetcountry","targetccode","Network.Connections","Territory"))
gtddrug2<-gtddrug2[!is.na(gtddrug$iyear),]
#baad merge
baad<- read_delim("BAAD_1_Lethality_Data.tab", "\t", escape_double = FALSE, trim_ws = TRUE)
gtddrug3<-merge(gtddrug2, baad, by.x="gname", by.y="group")

#CINC merge
CINC<-read.csv("NMC_5_0.csv")
gtddrug4<-merge(gtddrug3,CINC, by.x=c("targetccode", "iyear"), by.y=c("ccode","year"))

##Polity data merge
polity<-read.csv("p4_v2017.csv")
gtddrug5<-merge(gtddrug4,polity, by.x=c("targetccode", "iyear"), by.y= c("ccode","year"))


##Create Democracy measure
gtddrug5$democracy
gtddrug5$democracy[gtddrug5$polity2>=7]<-1
gtddrug5$democracy[is.na(gtddrug5$democracy)]<-0

#Subset
drugdata2<-subset(gtddrug5, select = c("gname","iyear","imonth","eventid","targetccode","sophisticated",
                                       "cowmastercountry","democracy", "ordsize", "PureRelig",
                                       "Network.Connections","terrStrong","statespond","cinc",
                                       "Drug1","Harvest1","Harvest2","Harvest3","narcotics" ))
names(drugdata2)[names(drugdata2) == 'eventid'] <- 'numattack'
drugdata2$terrStrong[is.na(drugdata2$terrStrong)]<-0
drugdata2$statespond[is.na(drugdata2$statespond)]<-0

#merge drug temp data.
drugdata2$fips<-countrycode(drugdata2$cowmastercountry, "country.name", "fips")


countryfiles<-as.list(list.files("./countryfiles/"))
countryfile<-countryfiles[1]
data<- read_delim(paste0("./countryfiles/",countryfile),  " ", escape_double = FALSE, trim_ws = TRUE)

readdata<-function(countryfile){
  country<-read_delim(paste0("./countryfiles/",countryfile),  " ", escape_double = FALSE, trim_ws = TRUE)
  return(country)
 }

mydata1<-do.call(rbind, lapply(countryfiles,readdata))

mydata<-aggregate(.~MONTH+YEAR+fips,mydata1 , FUN= mean)

DATA<-merge(drugdata2, mydata, by.x=c("imonth","iyear","fips"), by.y=c("MONTH", "YEAR", "fips"))

#number of days of ideal drug tempurature for specific drug
              
DATA$idealtemp<-NA             
DATA$idealtemp[(DATA$Drug1=="Opium"& !is.na(DATA$Drug1))]<-DATA$opium_t           
DATA$idealtemp[(DATA$Drug1=="Marijuana"& !is.na(DATA$Drug1))]<-DATA$mj_t                 
DATA$idealtemp[(DATA$Drug1=="Cocaine"& !is.na(DATA$Drug1))]<-DATA$coca_t              

##Month since harvest varaible
DATA$harv<-NA
DATA$harv[DATA$Harvest1==DATA$imonth]<-1
DATA$harv[DATA$Harvest2==DATA$imonth]<-1
DATA$harv[DATA$Harvest3==DATA$imonth]<-1

DATA$harv[is.na(DATA$harv)]<-0

#This starts count at 1 and also makes counts for countries with no harvest, should not be and issue
#since we will subset to only countries with narcotics when this variable is used
DATA2<-DATA%>%arrange(gname, iyear, imonth)%>%group_by(gname, idx = cumsum(harv == 1L))%>% 
  mutate(msh = row_number())%>%
  ungroup%>%select(-idx)
