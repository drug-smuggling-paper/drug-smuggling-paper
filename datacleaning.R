##Drug Smuggling and Terrorism

#Load data
library(data.table)
drugdata<-read.csv("drugdata.csv")
gtd<-fread("globalterrorismdb.csv")

#aggreagte gtd
gtdgroup<-aggregate(eventid~gname+iyear+imonth, data=gtd, FUN=length)

#merge
gtddrug<-merge(gtdgroup,drugdata, by.x="gname", by.y="GTD.name")

#subset
gtddrug2<-subset(gtddrug, select=c("gname","iyear","imonth","eventid","Network.Connections","Territory","Country","Drug1",
                                   "Plant1","Plant2","Harvest1","Harvest2","Harvest3"))
