acari <- read.table("acari.data",sep="\t")
colnames(acari) <- c("87Rb/86Sr","er.x","87Sr/86Sr","er.y")

plot(acari[,1],acari[,3],xlab=expression(""^87*Rb/""^86*Sr),ylab=expression(""^87*Sr/""^86*Sr),pch=15,cex=1.5)
izoch <- lm(acari[,3]~acari[,1]) 
print(izoch)

abline(izoch,lty="dashed")

age <- 1/1.42e-11*log(izoch$coeff[2]+1)
print(age/1e6)
