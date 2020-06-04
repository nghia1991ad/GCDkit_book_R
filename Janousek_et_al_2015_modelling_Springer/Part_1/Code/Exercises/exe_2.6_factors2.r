# Exercise 2.6
sazava <- read.table("sazava.data",sep="\t")
intrusion <- factor(sazava[,"Intrusion"])

tapply(sazava[,"SiO2"],intrusion,summary)

trace <- c("Rb","Sr","Ba","Zr")
aggregate(sazava[,trace],list(Rock=intrusion),mean,na.rm=TRUE)

by(sazava[,7:17],list(Rock=intrusion),summary)

#GCDkit solution
loadData("sazava.data")
groupsByLabel("Intrusion")

summarySingleByGroup("SiO2")

trace <- c("Rb","Sr","Ba","Zr")
summaryByGroup(trace)
summaryRangesByGroup(trace)

summaryByGroup(major)
