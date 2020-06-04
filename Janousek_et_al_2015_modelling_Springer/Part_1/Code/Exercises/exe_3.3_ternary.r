tri <- function(alab,blab,clab){ 
    sums <- apply(sazava[,c(alab,blab,clab)],1,sum)
    a <- sazava[,alab]/sums 
    b <- sazava[,blab]/sums
    plot(1-a-b/2,sqrt(3)*b/2,xlab="",ylab="",xlim=c(0,1),ylim=c(0,1),axes=FALSE,asp=1)
    # axes=FALSE: no plotting of axes; asp: aspect ratio
    x1 <- c(0,1,.5,0)
    y1 <- c(0,0,sqrt(3)/2,0)
    lines(x1,y1)
    text(-0.05,0,alab)
    text(1.05,0,clab)
    text(0.5,sqrt(3)/2+0.05,blab)
}
sazava <- read.table("sazava.data",sep="\t")
tri("Ba","Rb","Sr")

#GCDkit solution
loadData("sazava.data")
ternary("Ba","Rb","Sr",pch=1)
