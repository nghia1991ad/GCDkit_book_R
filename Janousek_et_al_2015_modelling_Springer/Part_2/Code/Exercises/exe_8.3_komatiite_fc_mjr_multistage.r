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
cs<-m.ol*ol+m.cpx*cpx       # cumulate composition
cl2<-(cl1-cs*fc2)/(1-fc2)  # composition of the residual liquid

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

# Plot
plot(c0[1],c0[2],xlab=expression(SiO[2]),ylab="CaO",xlim=c(45,60),ylim=c(4,11),pch=16,cex=2)
arrows(c0[1],c0[2],cl1[1],cl1[2],col="darkred",lwd=1.5)
arrows(cl1[1],cl1[2],cl2[1],cl2[2],col="darkblue",lwd=1.5)
arrows(cl2[1],cl2[2],cl3[1],cl3[2],col="darkgreen",lwd=1.5)
