sazava <- read.table("sazava.data",sep="\t")

sazava <- sazava[,-(1:6)] 
result <- apply(sazava,2,mean,na.rm=TRUE)
round(result,2)

#,width = 7,height=5
boxplot(sazava[,"Sr"],xlab="Sr", ylab="ppm")
summary(sazava[,"Sr"])

oxides <- c("SiO2","MgO","CaO","Na2O","K2O","P2O5")
pairs(sazava[,oxides])
