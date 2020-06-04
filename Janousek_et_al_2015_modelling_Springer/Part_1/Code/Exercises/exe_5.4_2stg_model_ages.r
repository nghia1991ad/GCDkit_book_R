# Needed only when the data are to be newly loaded:
izo <- read.table("cbpizo2.data",sep="\t",check.names=F)

DMLHAGE <- function(data,age){
    R <- data[,"147Sm/144Nd"]
    I <- data[,"143Nd/144Nd"]
    IDM <- 0.513151
    RDM <- 0.219
    RCC <- 0.12
    lambda <- 6.54e-12
    A <- I-(exp(lambda*age*1e6)-1)*(R-RCC)-IDM
    B <- RCC-RDM
    X <- 1/lambda*log(A/B+1)/10^9
    names(X) <- rownames(data)
return(X)
}

print(round(DMLHAGE(izo,350),2))
