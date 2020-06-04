# Create an empty plot (type="n"),with the correct range
plot(1,type="n",xlim=c(0,1),ylim=c(0.001,100),log="y",xlab="F",ylab=expression(C[L]/C[0]))
dd <- c(0.01,0.1,0.5,2,5) # Bulk distribution coefficients
for (i in 1:length(dd)){
    # Calculate and plot the fractional crystallization model
    curve(x^(dd[i]-1),from=0,to=1,lty="solid",lwd=2,col=i,add=TRUE)
    # Calculate and plot the batch crystallization model
    curve(1/(x+dd[i]*(1-x)),from=0,to=1,lty="dashed",  col=i,add=TRUE)
}
# Add a horizontal grid and legend
abline(h=10^(-3:2),col="gray")
legend("topright",pch=16,col=1:length(dd),legend=dd,title="D",bg="white")
