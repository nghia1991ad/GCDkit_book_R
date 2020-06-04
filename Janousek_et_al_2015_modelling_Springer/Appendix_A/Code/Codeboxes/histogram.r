sazava<-read.table("sazava.data",sep="\t")
hist(sazava[,"Sr"],xlab="Sr",ylab="frequency",xlim=c(100,700),col="darkred",density=5,angle=45)
box()
