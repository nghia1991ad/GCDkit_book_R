sazava  <-  read.table("sazava.data",sep="\t")
plot(sazava[,"SiO2"],sazava[,"Ba"],xlab=expression(SiO[2]),cex=1.5,ylab="Ba",pch=16,main="Sazava",xlim=c(45,75))
abline(h=seq(0,1500,500),lty="dotted",col="gray")
abline(v=seq(40,80,10),lty="dotted",col="gray")
text(sazava[,"SiO2"],sazava[,"Ba"],rownames(sazava),pos=4,col="red")

ee  <-  lm(sazava[,"Ba"]~sazava[,"SiO2"]) # ee = arbitrary name of variable with least-square fit
print(ee)
abline(ee,lwd=2,lty="dashed",col="blue")
summary(ee)
