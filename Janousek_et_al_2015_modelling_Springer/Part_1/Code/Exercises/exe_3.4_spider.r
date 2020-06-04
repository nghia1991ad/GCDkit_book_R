x <- read.table("boynton.data",sep=",")
chondrite <- as.numeric(x) # conversion to numeric vector
names(chondrite) <- names(x) 

norm <- function(x,chon){
    z <- t(x[,names(chon)])/chon 
    return(z)
}

sazava <- read.table("sazava.data",sep="\t")
y <- norm(sazava,chondrite)
plot(y[,"Po-1"],type="o",log="y",axes=FALSE,xlab="",ylab="REE/chondrite",ylim=c(0.1,100),col="darkgreen")
axis(1,1:length(chondrite),labels=names(chondrite),cex.axis=0.75)
axis(2,cex.axis=0.75)
points(y[,"Po-4"],col="blue")
lines(y[,"Po-4"],col="blue")
abline(h=(10^(-1:3)),lty="dashed")
box()
