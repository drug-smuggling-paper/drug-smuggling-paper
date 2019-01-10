##Drug Smuggling and Terrorism

#Load data
library(data.table)
library(readr)
drugdata<-read.csv("drugdata.csv")
gtd<-fread("globalterrorismdb.csv")

#aggreagte gtd
gtdgroup<-aggregate(eventid~gname+iyear+imonth, data=gtd, FUN=length)

#merge and add narcotics dummy
drugdata$narcotics<-1
gtddrug<-merge(gtdgroup,drugdata, by.x="gname", by.y="GTD.name",all = TRUE)
gtddrug$narcotics[is.na(gtddrug$narcotics)]<-0

#subset
gtddrug2<-subset(gtddrug, select=c("gname","iyear","imonth","eventid","Network.Connections","Territory","Country","Drug1",
                                   "Plant1","Plant2","Harvest1","Harvest2","Harvest3","narcotics"))

#baad merge
baad<- read_delim("BAAD_1_Lethality_Data.tab", "\t", escape_double = FALSE, trim_ws = TRUE)
gtddrug3<-merge(gtddrug2, baad, by.x="gname", by.y="group")

