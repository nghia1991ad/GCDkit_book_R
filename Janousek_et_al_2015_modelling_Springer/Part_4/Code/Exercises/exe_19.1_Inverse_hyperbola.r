windows()
z <- read.table("Martinique.data",sep="\t")

# Fitting the mixing hyperbola
A <- cbind(1,z[,2],z[,1])           # data matrix [Eq. (17.2)]
y <- z[,1]*z[,2]                    # left-hand vector [Eq. (17.2)]
xa <- lsfit(A,y,intercept=FALSE)    # least-square calculation
xa <- xa$coeff
names(xa) <- NULL                   # process the resulting list

# Determining asymptotes and curvature
x0 <- xa[2]; y0 <- xa[3]            # asymptotes [Eq. (17.2)]
q <- xa[1]+x0*y0                    # curvature [Eq. (17.2)]
print(x0)
print(y0)
print(q)

# Calculate coordinates of the hyperbola [Eq. (16.10)]
xx <- seq(min(z[,1]),max(z[,1]),((max(z[,1])-min(z[,1]))/100))
yy <- y0+q/(xx-x0)

# Plotting the mixing hyperbola
plot(xx,yy,type="l",xlab=expression(""^206*Pb/""^204*Pb),
ylab=expression(""^176*Hf/""^177*Hf),
xlim=c(min(z[,1])-0.01,x0+0.01),
ylim=c(min(z[,2])-0.00001,y0-0.00001))
points(z[,1],z[,2],cex=1.5,pch=19)  # add data points
abline(v=x0,lty="dashed")           # add asymptotes
abline(h=y0,lty="dashed")
