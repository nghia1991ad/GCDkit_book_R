sazava <- read.table("sazava.data",sep="\t")
windows(width=8,height=6)
par(mfrow=c(2,3))           # Split screen for 6 graphs
ee <- c("Al2O3","MgO","CaO","Na2O","K2O","P2O5")
for (f in ee){
    plot(sazava[,"SiO2"],sazava[,f],xlab="SiO2",ylab=f,pch=sazava[,"Symbol"],cex=1.5)
}

windows(width=8,height=6)
par(mfrow=c(2,3))
lab <- c("Al[2]*O[3]","MgO","CaO","Na[2]*O","K[2]*O","P[2]*O[5]")
for (f in 1:length(ee)){
    plot(sazava[,"SiO2"],sazava[,ee[f]],xlab=expression(SiO[2]),ylab=parse(text=as.expression(lab[f])),pch=sazava[,"Symbol"],cex=1.5)
}

# GCDkit solution
loadData("sazava.data")
multiple("SiO2","Al2O3,MgO,CaO,Na2O,K2O,P2O5")
