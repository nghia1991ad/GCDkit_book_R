sazava <- read.table("sazava.data",sep="\t")

# data(sazava) # Alternative in GCDkit

# loadData("sazava.data") # Another alternative
# sazava <- cbind(labels,WR)

colnames(sazava)

sazava["Sa-1","MgO"]

sazava <- sazava[,-(1:6)] # Stripping first six columns
sazava[c("Po-1","Po-4"),]

sum(sazava[,"Na2O"])

silica <- sazava[,"SiO2"]
names(silica) <- rownames(sazava)
names(sort(silica))[1:5]
names(rev(sort(silica)))[1:5]

apply(sazava,2,mean,na.rm=TRUE)

x <- cbind(sazava[,"SiO2"],sazava[,"MgO"],sazava[,"Na2O"]/sazava[,"K2O"])
colnames(x) <- c("SiO2","MgO","Na2O/K2O")
rownames(x) <- rownames(sazava)
x
