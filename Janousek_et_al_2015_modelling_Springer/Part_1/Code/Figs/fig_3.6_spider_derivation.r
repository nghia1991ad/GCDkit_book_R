sazava <- read.table("sazava.data",sep="\t")
sazava<-as.matrix(sazava[,-(1:4)])
x <- read.table("boynton.data",sep=",")
chondrite <- as.numeric(x)  # conversion to numeric vector
names(chondrite) <- names(x) 


windows()                   # Empty window
par(mfrow=c(2,2))           # Split screen for 2 graphs
par(mar=c(3,4,1,1))         # Outer margins for each of the graphs

norm <- function(x,chon){
    z <- t(x[,names(chon)])/chon 
    return(z)
}

plot.sp<-function(x,y.txt="REE/chondrite",col="black",main=NULL){
    plot(x,type="o",log="y",axes=FALSE,xlab="",ylab=y.txt,col=col,pch=15,main=main,cex.main=0.75) 
    axis(1,1:length(chondrite),labels=names(chondrite),cex.axis=0.65)
    axis(2,cex.axis=1)
    abline(h=(10^(-1:3)),lty="dashed")
    box()
}

plot.sp(chondrite,"(ppm)",col="darkblue",main="Chondrite (Boynton 1984)")
plot.sp(sazava["Po-1",names(chondrite)],"(ppm)",col="red",main="Po-1 (original)")

y <- norm(sazava,chondrite)
plot.sp(y[,"Po-1"],col="darkorchid4",main="Po-1 (normalized)")

# GCDkit solution for the last plot
ee<-spider(sazava["Po-1",],"Boynton",1,100,pch=15,col="darkorchid4",offset=T)
