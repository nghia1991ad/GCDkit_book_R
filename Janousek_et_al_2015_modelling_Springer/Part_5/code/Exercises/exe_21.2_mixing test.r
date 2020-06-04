x <- read.table("kozamix.data",sep="\t")
mix1 <- x[,1]-x[,3]                             # C1-C2, x-coordinate
mix2 <- x[,2]-x[,3]                             # CM-C2, y-coordinate
plot(mix1,mix2,xlim=c(-10,16),ylim=c(-10,16),pch=16,cex.lab=1.3,xlab=expression(C[1]-C[2]),ylab=expression(C[M]-C[2]))
abline(h=0)                                     # horizontal line through 0
abline(v=0)                                     # vertical line
text(mix1,mix2,rownames(x),adj=c(1,0),pos=3)    # label by oxide names
lq <- lsfit(mix1,mix2,intercept=FALSE)          # calculate least squares
abline(lq,lty="dashed")                         # plot the best fit line
print(lq$coeff)                                 # slope = f1 [Eq. (21.13)]
