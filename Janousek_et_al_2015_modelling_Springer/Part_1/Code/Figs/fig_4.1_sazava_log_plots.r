sazava <- read.table("sazava.data",sep="\t")
windows(width=10,height=5,pointsize=14) # Empty window of correct size
par(mfrow=c(1,2))                       # Split screen for 2 graphs
par(mar=c(4,4,1,1))                     # Outer margins for each of the graphs

# Plot 1
plot(sazava[,"Sr"],sazava[,"Rb"],xlim=c(1,1000),ylim=c(1,1000),xlab="Sr",ylab="Rb",pch=16,col="blue",cex=1.5,xaxs="i",yaxs="i",log="")
sapply(c(seq(0.1,1,0.1),1:10),function(i)abline(0,i,lty="dashed"))

# Plot 2
plot(sazava[,"Sr"],sazava[,"Rb"],xlim=c(1,1000),ylim=c(1,1000),xlab="Sr",ylab="Rb",pch=16,col="blue",cex=1.5,xaxs="i",yaxs="i",log="xy")
sapply(c(seq(0.1,1,0.1),1:10),function(i)abline(log10(i),1,lty="dashed"))
