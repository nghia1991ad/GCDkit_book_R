windows(width=10,height=6)
c1<- 150; i1 <- 0.715   #schist composition
c2 <- 600; i2 <- 0.703  #basalt composition

# Plot mixing
par(mfrow=c(1,2))   #prepare two-graph layout
f1 <- seq(0,1,by=0.05)  #proportions of schist in mixture
cm <- c1*f1+(1-f1)*c2
names(cm) <- f1 #conc. in mix [Eq.(16.2)]
im <- i1*c1*f1/cm+i2*c2*(1-f1)/cm   #87Sr/86Sr in mix [Eq. (16.1)]
srlab <- expression(""^87*Sr/""^86*Sr)
plot(cm,im,xlab="Sr (ppm)",ylab=srlab,type="b",pch=19)
plot(1/cm,im,xlab="1/Sr (ppm)",ylab=srlab,type="b",pch=19)

#Calculate 20% mixing
f1 <- 0.2   #mixing proportion
im <- (i1*c1*f1+i2*c2*(1-f1))/(c1*f1+c2*(1-f1)) # 87Sr/86Sr [Eq. (16.3)] 
print(im)

#Calculate mixing proportion (reverse task)
im <- 0.710 #87Sr/86Sr of the mixture
f1 <- c2*(i2-im)/(im*(c1-c2)-i1*c1+i2*c2)   #calc. f1 [Eq.(16.5)]
print(f1)
