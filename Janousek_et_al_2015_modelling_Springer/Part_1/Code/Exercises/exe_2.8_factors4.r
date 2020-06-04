sazava <- read.table("sazava.data",sep="\t")

silica <- cut(sazava[,"SiO2"],breaks=c(0,45,52,63,100),labels=c("U","B","I","A"))

# Exercise 2.8
intrusion <- factor(sazava[,"Intrusion"])
table(intrusion)
table(silica)
table(intrusion,silica)
