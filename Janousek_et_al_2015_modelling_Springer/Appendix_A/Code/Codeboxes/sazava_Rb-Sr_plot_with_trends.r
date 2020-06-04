sazava <- read.table("sazava.data",sep="\t")
plot(sazava[,"Rb"],sazava[,"Sr"],xlab="Rb (ppm)",ylab="Sr (ppm)",pch=sazava[,"Symbol"],cex=2,xlim=c(0,70),ylim=c(0,650),xaxs="i",yaxs="i") 
abline(0,5, col="red",lwd=1.5,lty="dashed")
curve(x^2,add=TRUE,col="blue",lwd=1.5,lty="dotted",from=0,to=25)
