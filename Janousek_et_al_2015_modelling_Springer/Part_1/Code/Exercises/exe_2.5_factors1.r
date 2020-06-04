# Exercise 2.5
sazava <- read.table("sazava.data",sep="\t")
intrusion <- factor(sazava[,"Intrusion"])
intrusion

levels(intrusion)

tapply(sazava[,"SiO2"],intrusion,mean)
tapply(sazava[,"Ba"],intrusion,mean)

tapply(sazava[,"Ba"],intrusion,is.na)
tapply(sazava[,"Ba"],intrusion,mean,na.rm=TRUE)
