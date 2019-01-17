##Drug Smuggling and Terrorism

#Load data
library(data.table)
library(readr)
drugdata<-read.csv("drugdata.csv")
gtd<-fread("globalterrorismdb.csv")

#aggreagte gtd
gtdgroup<-aggregate(eventid~gname+iyear+imonth, data=gtd, FUN=length)

#merge
drugdata$narcotics<-1
gtddrug<-merge(gtdgroup,drugdata, by.x="gname", by.y="GTD.name",all = TRUE)
gtddrug$narcotics[is.na(gtddrug$narcotics)]<-0

#subset
gtddrug2<-subset(gtddrug, select=c("gname","iyear","imonth","eventid","Network.Connections","Territory","Country","Drug1",
                                   "Plant1","Plant2","Harvest1","Harvest2","Harvest3","narcotics"))

#baad merge
baad<- read_delim("BAAD_1_Lethality_Data.tab", "\t", escape_double = FALSE, trim_ws = TRUE)
gtddrug3<-merge(gtddrug2, baad, by.x="gname", by.y="group")

#CINC merge
CINC<-read.csv("NMC_5_0.csv")
gtddrug4<-merge(gtddrug3,CINC, by.x=c("masterccode", "iyear"), by.y= c("ccode","year"))

##Polity data merge
polity<-read.csv("p4_v2017.csv")
gtddrug5<-merge(gtddrug4,polity, by.x=c("masterccode", "iyear"), by.y= c("ccode","year"))

gtddrug5$democracy
gtddrug5$democracy[gtddrug5$polity2>=7]<-1
gtddrug5$democracy[is.na(gtddrug5$democracy)]<-0

#Subset
drugdata2<-subset(gtddrug5, select = c("gname","iyear","imonth","eventid",
                                       "Country","democracy",
                                       "Network.Connections","Territory","statespond",
                                       "Drug1","Plant1","Plant2","Harvest1","Harvest2","Harvest3","narcotics" ))
names(drugdata2)[names(drugdata2) == 'eventid'] <- 'numattack'
onlydruggroups<-subset(drugdata2, drugdata2$narcotics==1)
