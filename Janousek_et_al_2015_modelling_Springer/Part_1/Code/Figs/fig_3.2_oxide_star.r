sazava <- read.table("sazava.data",sep="\t")
oxides <- c("TiO2","Al2O3","FeO","Fe2O3","MnO","MgO","CaO","Na2O","K2O","P2O5")

ee <- sapply(rownames(sazava),function(i){
    z<-100*sazava[i,oxides]/(100-sazava[i,"SiO2"])
    return(z)
    }
)
oxide_star<-t(ee)

windows(width = 10,height=6)
par(mfrow=c(1,2))
# Panel a
plot(sazava[,"SiO2"],sazava[,"Al2O3"],col=sazava[,"Colour"],pch=sazava[,"Symbol"],cex=1.5,xlim=c(45,100),ylim=c(0,40),xaxs="i",yaxs="i",xlab=expression(SiO[2]),ylab=expression(Al[2]*O[3]))

# Forbidden zone
polygon(c(100,60,100,100),c(0,40,40,0),density=10,col="gray")

# Panel b
plot(sazava[,"SiO2"],oxide_star[,"Al2O3"],col=sazava[,"Colour"],pch=sazava[,"Symbol"],cex=1.5,xlim=c(45,100),ylim=c(0,60),xaxs="i",yaxs="i",xlab=expression(SiO[2]),ylab=expression(Al[2]*O[3]*"*"))
