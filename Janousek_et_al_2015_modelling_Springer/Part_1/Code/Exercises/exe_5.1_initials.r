# Exercise 5.1
izo <- read.table("cbpizo.data",sep="\t",check.names=F)
sr <- izo[,1]/izo[,2]*(2.6939+0.2832*izo[,3])
nd <- izo[,4]/izo[,5]*(0.53151+0.14252*izo[,6])
izo <- cbind(izo,sr,nd)
colnames(izo)[7:8] <- c("87Rb/86Sr","147Sm/144Nd")
print(izo[,7:8])

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

izo <- cbind(izo,initial(izo,"Sr",350),initial(izo,"Sr",300),initial(izo,"Nd",350),initial(izo,"Nd",300))
colnames(izo)[9:12] <- c("87Sr/86Sr.350","87Sr/86Sr.300","143Nd/144Nd.350","143Nd/144Nd.300")
print(izo[,9:12])

age <- 1/1.42e-11*log((izo["Koz-2","87Sr/86Sr"]-0.705)/izo["Koz-2","87Rb/86Sr"]+1)
print(age/1e6)
