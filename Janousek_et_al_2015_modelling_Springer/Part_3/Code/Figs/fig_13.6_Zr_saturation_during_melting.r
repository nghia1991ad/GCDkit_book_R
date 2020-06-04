# Initial parameters of the model (x = F)
xmax<-0.6
cl <- 100

windows(width=10,height=6)
par(mfrow=c(1,2))

# Plot 1 source > melt
c0 <- 150
plot(1, type="n", xlim=c(0,xmax), ylim=c(0,250),xlab="F", ylab="Zr (ppm)",xaxs="i",yaxs="i")

# Residue
curve((c0-cl*x)/(1-x), from=0, to=xmax, lty="solid", col="darkgray", lwd=2, add=TRUE)

# Melt
lines(c(0,xmax),c(cl,cl),col="red",lty="solid",lwd=2) 


# Plot 2 source < melt
c0 <- 40
plot(1, type="n", xlim=c(0,xmax), ylim=c(0,250),xlab="F", ylab="Zr (ppm)",xaxs="i",yaxs="i")

# Residue
curve((c0-cl*x)/(1-x), from=0, to=xmax, lty="solid", col="darkgray", lwd=2, add=TRUE)
rest.zero<-c0/cl

# Melt
lines(c(0,rest.zero),c(cl,cl),col="red",lty="solid",lwd=2) 
abline(v=rest.zero,lty="dotted")
curve(c0/x, from=rest.zero, to=xmax, lty="solid", col="red", lwd=2, add=TRUE)
