sazava<-read.table("sazava.data",sep="\t")
oxides<-c("MgO","CaO","Na2O","K2O")
pairs(sazava[,oxides],pch=15,col="darkred")
