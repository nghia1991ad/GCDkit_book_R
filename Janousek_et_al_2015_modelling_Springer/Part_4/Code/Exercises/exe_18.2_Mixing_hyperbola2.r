windows()
cx1 <- 150; cx2 <- 600          # Sr, schist and basalt
ix1 <- 0.715; ix2 <- 0.703      # 87Sr/86Sr, schist and basalt
f1 <- seq(0,1,by=0.05)          # prop. of schist in mixture
cmx <- cx1*f1+(1-f1)*cx2        # Sr conc. mix [Eq. (16.2)]
imx <- ix1*cx1*f1/cmx+ix2*cx2*(1-f1)/cmx 
                                # 87Sr/86Sr mix [Eq. (16.1)]

cy1 <- 20; cy2 <- 2             # Nd in schist and basalt
iy1 <- 0.511; iy2 <- 0.513      # 143Nd/144Nd, schist/basalt
cmy <- cy1*f1+(1-f1)*cy2        # Nd conc., mix [Eq. (16.2)]
imy <- iy1*cy1*f1/cmy+iy2*cy2*(1-f1)/cmy 
                                # 143Nd/144Nd mix [Eq. (16.1)]

# Prepare results table
res <- cbind(cmx,imx,cmy,imy)
rownames(res) <- f1
colnames(res) <- c("Sr","87Sr/86Sr","Nd","143Nd/144Nd")
print(res)

# Plot the mixing hyperbola
plot(imx,imy,xlab=expression(""^87*Sr/""^86*Sr),ylab=expression(""^143*Nd/" "^144*Nd),type="b",xlim=c(0.7023,0.7152),ylim=c(0.5109,0.5131),pch=19)

# Calculate and plot asymptotes
alpha <- (cx2/cy2)/(cx1/cy1)    # calc. alpha [Eq. (16.8)]
x0 <- (ix1-alpha*ix2)/(1-alpha) # calc. asymptotes [Eq. (16.14)]
y0 <- (iy2-alpha*iy1)/(1-alpha) 
print(x0)
print(y0)
abline(v=x0,lty="dashed")       # draw asymptotes
abline(h=y0,lty="dashed")
