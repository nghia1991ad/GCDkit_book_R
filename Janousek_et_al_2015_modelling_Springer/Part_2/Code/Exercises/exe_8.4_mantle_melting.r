# Build matrix with two columns of the input data, fill along rows
qq <- matrix(c(15,0.7,4,0.17,1,0.4),ncol=2,byrow=TRUE)
colnames(qq) <- c("Al2O3","TiO2")
rownames(qq) <- c("basalt","fertile","depleted")

# Plot the diagram
plot(qq[,"Al2O3"],qq[,"TiO2"],xlim=c(0,16),ylim=c(0,1),xlab=expression(Al[2]*O[3]),ylab=expression(TiO[2]),xaxs="i",yaxs="i")# no extra space at axes
text(qq[,1],qq[,2]+0.03,rownames(qq),adj=c(0.5,0))
ff<-0.1                     # degree of melting
residue <-(qq["fertile",]-ff*qq["basalt",])/(1-ff) # [Eq.(8.3)]
points(residue[1],residue[2],pch=8,col="red")
print(residue,3)

# Define the melting trend by slope and intercept
b<-(qq["basalt",2]-qq["fertile",2])/(qq["basalt",1]-qq["fertile",1])# slope of the melting trend
a<- qq["basalt",2]-b*qq["basalt",1]
abline(a,b,lty="dashed")    # plot a straight line
