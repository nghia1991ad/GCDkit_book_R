sazava <- read.table("sazava.data",sep="\t")
MW <- c(71.85,40.31,101.96,56.08,61.98,94.20)
oxides <- c("FeO","MgO","Al2O3","CaO","Na2O","K2O")
names(MW) <- oxides
# Transpose as the division of a matrix by a vector 
# proceeds along columns, not rows.
mol <- t(sazava[,oxides])/MW[oxides]

mgno <- function(){
    mg <- 100*mol["MgO",]/(mol["FeO",]+mol["MgO",])
    return(mg)
}

ank <- function(){
    ANK <- mol["Al2O3",]/(mol["Na2O",]+mol["K2O",])
    return(ANK)
}

acnk <- function(){
    ACNK <- mol["Al2O3",]/(mol["Na2O",]+mol["K2O",]+mol["CaO",])
    return(ACNK)
}

# Calculate the indexes
x <- cbind(mgno(),acnk(),ank())
colnames(x) <- c("mg.no","A/CNK","A/NK")
x

major <- c("SiO2","TiO2","Al2O3","Fe2O3","FeO","MnO","MgO","CaO","Na2O","K2O","P2O5")
sums <- apply(sazava[,major],1,sum)
anh <- sazava[,major]/sums*100
anh

# GCDkit solution
loadData("sazava.data")
WR[,c("mg#","A/CNK","A/NK")]
print(WRanh)
