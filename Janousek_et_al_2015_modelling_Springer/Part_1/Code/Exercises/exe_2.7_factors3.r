# Exercise 2.7
sazava <- read.table("sazava.data",sep="\t")
silica <- cut(sazava[,"SiO2"],breaks=c(0,45,52,63,100),labels=c("U","B","I","A"))
silica

acidity <- as.vector(silica)
names(acidity) <- rownames(sazava)
acidity

# GCDkit solution
loadData("sazava.data")
cutMy("SiO2",c(0,45,52,63,100), c("U","B","I","A"))
