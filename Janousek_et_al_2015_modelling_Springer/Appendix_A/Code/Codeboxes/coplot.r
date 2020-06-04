sazava<-read.table("sazava.data",sep="\t")
coplot(sazava[,"CaO"]~ sazava[,"SiO2"]| sazava[,"Intrusion"],cex=1.5,xlab=expression(SiO[2]),ylab="CaO",pch=sazava[,"Symbol"],col=sazava[,"Colour"])
