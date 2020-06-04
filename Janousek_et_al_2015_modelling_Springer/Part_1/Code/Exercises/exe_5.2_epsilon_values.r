# Needed from Exercise 5.1
izo <- read.table("cbpizo.data",sep="\t",check.names=F)
sr <- izo[,1]/izo[,2]*(2.6939+0.2832*izo[,3])
nd <- izo[,4]/izo[,5]*(0.53151+0.14252*izo[,6])
izo <- cbind(izo,sr,nd)
colnames(izo)[7:8] <- c("87Rb/86Sr","147Sm/144Nd")

initial <- function(data,system="Sr",age){
    # data = matrix to be recalculated
    # system = "Sr" or "Nd", age = age in Ma
    lambda <- c(1.42*10^-11,6.54*10^-12)
    names(lambda) <- c("Sr","Nd")
    R <- cbind(data[,"147Sm/144Nd"],data[,"87Rb/86Sr"])
    colnames(R) <- c("Nd","Sr")
    I <- cbind(data[,"143Nd/144Nd"],data[,"87Sr/86Sr"])
    colnames(I) <- c("Nd","Sr")
    X <- I[,system]-R[,system]*(exp(lambda[system]*age*10^6)-1)
    return(X)
}


# Exercise 5.2
epsilon <- function(data,age){
    RCHUR <- 0.1967 
    ICHUR <- 0.512638 # Jacobsen and Wasserburg (1980)
    CHUR <- ICHUR-RCHUR*(exp(6.54e-12*age*10^6)-1)
    X <- (initial(data,"Nd",age)/CHUR-1)*10^4
    names(X) <- rownames(data)
    return(X)
}

print(round(epsilon(izo,350),2))

plot(initial(izo,age=350),epsilon(izo,350),xlab=expression(" "^87*Sr/""^86*Sr[i]),ylab=expression(epsilon[Nd]^i),pch=11,cex=1.5,cex.lab=1.5,cex.axis=1.5)
