windows(9,6)
par(mfrow=c(1,2))#prepare two-graph layout

# EXERCISE
c0 <- c(48.14,5.99)         # primitive magma
names(c0) <- c("SiO2","CaO")
ol <- c(40.6,0)             # olivine
cpx <- c(51.2,20.73)        # clinopyroxene

# First stage
fc1 <- 0.4                  # degree of fractionation
cl1 <- (c0-ol*fc1)/(1-fc1)  # composition of the residual liquid

# Second stage
fc2 <- 0.15                 # degree of fractionation
m.ol <- 0.6                 # proportion of olivine
m.cpx <- 0.4                # proportion of clinopyroxene
cum<-m.ol*ol+m.cpx*cpx      # cumulate composition
cl2<-(cl1-cum*fc2)/(1-fc2)  # composition of the residual liquid

# Third stage
fc3 <- 0.2                  # degree of fractionation
cl3 <- (cl2-cpx*fc3)/(1-fc3)# composition of the residual liquid

# Results
res <- cbind(c0,cl1,cl2,cl3)
print(round(res,3))# print the results, rounded off

# Degree of fractionation
step1 <- fc1
step2 <- (1-step1)*fc2
step3 <- (1-step1-step2)*fc3
cat("Total degree of fractionation is",step1+step2+step3,"\n")

# Plot 1
plot(c0[1],c0[2],xlab=expression(SiO[2]),ylab="CaO",xlim=c(45,60),ylim=c(4,11),pch=16,cex=2)
arrows(c0[1],c0[2],cl1[1],cl1[2],col="darkgreen",lwd=1.5)
arrows(cl1[1],cl1[2],cl2[1],cl2[2],col="darkblue",lwd=1.5)
arrows(cl2[1],cl2[2],cl3[1],cl3[2],col="brown",lwd=1.5)
legend("topleft","a",bty = "n",inset = 0,adj = 1)

# CONCEPT
screen(2)
plot(c0[1],c0[2],xlab=expression(SiO[2]),ylab="CaO",xlim=c(40,60),ylim=c(0,21),pch=16,cex=2,type="n")
rect(45-(60-45)*0.04,4-(11-6)*0.04,60+(60-45)*0.04,11+(11-6)*0.04,col="gray80",border=NA)


arrows(c0[1],c0[2],cl1[1],cl1[2],col="darkgreen",lwd=2)
arrows(cl1[1],cl1[2],cl2[1],cl2[2],col="darkblue",lwd=2)
arrows(cl2[1],cl2[2],cl3[1],cl3[2],col="brown",lwd=2)

points(ol[1],ol[2],pch=8,cex=2,col="darkgreen")
text(ol[1],ol[2]+0.2,"Ol",col="darkgreen",adj=c(0.5,0))

points(cum[1],cum[2],pch=8,cex=2,col="darkblue")
text(cum[1],cum[2]+0.2,"0.6Ol +\n0.4Cpx",col="darkblue",adj=c(0.5,0))

points(cpx[1],cpx[2],pch=8,cex=2,col="brown")
text(cpx[1],cpx[2]+0.2,"Cpx",col="brown",adj=c(0.5,0))

lines(rbind(ol,c0),lty="dashed")
lines(rbind(cum,cl1),lty="dashed")
lines(rbind(cpx,cl2),lty="dashed")

points(c0[1],c0[2],pch=16,cex=1.5)
text(c0[1],c0[2]-0.4,"Primary\nmelt",adj=c(0.5,1))

points(cl3[1],cl3[2],pch=1,cex=1.5)
text(cl3[1],cl3[2]-0.4,"Fractionated\nmelt",adj=c(0.5,1))


# Various mixtures of Ol and Cpx
f<-seq(0,1,by=0.1)
a<-(cpx[2]-ol[2])/(cpx[1]-ol[1])
b<-cpx[2]-a*cpx[1]
equation<-paste(a,"*x+",b,sep="")
x<-f*ol[1]+(1-f)*cpx[1]
trendTicks(equation,x,1,min(x),max(x),lty="solid",col="gray20",lwd=1,arrow=FALSE,text=FALSE)
legend("topleft","b",bty = "n",inset = 0,adj = 1)
