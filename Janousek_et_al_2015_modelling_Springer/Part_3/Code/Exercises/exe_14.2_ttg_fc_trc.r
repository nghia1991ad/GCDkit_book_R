x <- read.table("ttg.data",sep="\t")
x <- as.matrix(x)               # transform dataframe to matrix
c0 <- x[,1]                     # parental magma composition
kd <- x[,-1]                    # table of distribution coefficients
fc <- 0.3                       # degree of fractionation
ff <- 1-fc                      # fraction of the melt left 
m1 <- c(0.49,0.49,0.02,0)       # mineral props in cumulate (1)
dd1 <- kd%*%m1                  # bulk distrib. coeff. [Eq.(10.4)]
names(m1) <- colnames(dd1)
cl1 <- c0*ff^(dd1-1)            # melt composition
cs1 <- dd1*cl1                  # instantaneous solid
csavg1 <- c0*(1-ff^dd1)/(1-ff)  # average solid

m2 <- c(0.485,0.485,0.02,0.01)  # mineral props in cumulate (2)
dd2 <- kd%*%m2                  # bulk distrib. coeff. [Eq. (10.4)]
names(m2) <- colnames(dd2)
cl2 <- c0*ff^(dd2-1)            # melt composition
cs2 <- dd2*cl2                  # instantaneous solid
csavg2 <- c0*(1-ff^dd2)/(1-ff)  # average solid
result <- cbind(c0,dd1,dd2,cs1,csavg1,cl1,cs2,csavg2,cl2)
colnames(result) <- c("C0","D1","D2","CS1","CSavg1","CL1","CS2","CSavg2","CL2")
print(round(result,2))

data <- t(result[,c("C0","CL1","CL2")])
spider(data,"Boynton",0.1,1000,pch=c(15,1,16),col=c("black","red","blue"))
