# Needed only when the data are to be newly loaded:
izo <- read.table("cbpizo2.data",sep="\t",check.names=F)

DMAGE <- function(data){
    IDM <- 0.513114
    RDM <- 0.222
    lambda <- 6.54e-12
    R <- data[,"147Sm/144Nd"]
    I <- data[,"143Nd/144Nd"]
    X <- 1/lambda*log((I-IDM)/(R-RDM)+1)/10^9
    names(X) <- rownames(data)
    return(X)
}
print(round(DMAGE(izo),2))
