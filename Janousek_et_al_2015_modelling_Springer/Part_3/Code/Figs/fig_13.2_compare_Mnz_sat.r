
###Figure 13.2 Monazite sat 
###  Montel vs. Kelsey vs. Stepanov

##Definitions, variables
MW.REE<-c(138.9055,140.12,140.9077,144.24,151.4,154.25)
W.R<-c(-260,-116,31,177,460,750)
X.mnz<-c(0.165,0.351,0.041,0.155,0.03,0.021)
names(MW.REE)<-c("La","Ce","Pr","Nd","Sm","Gd")
names(W.R)<-c("La","Ce","Pr","Nd","Sm","Gd")
names(X.mnz)<-c("La","Ce","Pr","Nd","Sm","Gd")

DM<-c(1.01,1.09,1.23)
FMK<-c(1.57,2.23,2.40)
wat<-c(3,2.5,2)
cols<-c("red","purple","royalblue")
Temp<-seq(600,1200,10)

Press<-5
X.lree.mnz<-X.mnz["La"]+X.mnz["Ce"]+X.mnz["Pr"]+X.mnz["Nd"]+X.mnz["Sm"]

## Calculations
reet.M.unco<-NULL
reet.M.part<-NULL
reet.M.glob<-NULL
lree.S<-NULL
reet.K<-NULL

for(i in 1:3){
  
  # Montel - full REE
  ee<-exp(9.5+2.34*DM[i]+0.3879*sqrt(wat[i])-13318/(Temp+273))
  reet.M.unco<-cbind(reet.M.unco,ee)
  
  # Montel -- REE part
  e.ree<-rep(0,length(Temp))
  
  for(e in names(MW.REE)){
     qq<-X.mnz[e]*exp(W.R[e]/(Temp+273))*(reet.M.unco[,i])
     e.ree<-cbind(e.ree,qq)
  }
  e.ree<-e.ree[,-1]
  colnames(e.ree)<-c("La","Ce","Pr","Nd","Sm","Gd")
  
  ee<-e.ree%*%MW.REE
  reet.M.part<-cbind(reet.M.part,ee)

  # Montel, corrected
  reet.M<-reet.M.unco[,i]*0.83
  
  foo<-rep(0,length(Temp))
  n.ree.t<-rowSums(e.ree)
  for(e in names(MW.REE)){
    qq<-e.ree[,e]/n.ree.t*reet.M
    foo<-cbind(foo,qq)
  }
  
  ee<-foo[,-1]%*%MW.REE
  reet.M.glob<-cbind(reet.M.glob,ee)

  # Stepanov
  ee<-exp(16.16+0.23*sqrt(wat[i])-11494/(Temp+273)-19.4*Press/(Temp+273)+log(X.lree.mnz))
  lree.S<-cbind(lree.S,ee)
  
  # Kelsey
  ee<-566794/(exp(-310/(Temp+273)-1.324*FMK[i]+7.5852))
  reet.K<-cbind(reet.K,ee)
}

## Plots: fig. 13.2
windows(11,6)
par(mfrow=c(1,2))

# plot 1
plot(0,-2,col="ivory",pch=17,xlim=c(7,10),ylim=c(100,10000),log="y",xlab="10000/T (K)", ylab="REE (ppm)")
for(i in 1:3){
  tt<-10000/(Temp+273)
  lines(tt,reet.M.part[,i],col=cols[i],lwd=2,lty="dashed")
  lines(tt,reet.M.glob[,i],col=cols[i],lwd=2,lty="dotted")
  lines(tt,lree.S[,i],col=cols[i],lwd=2,lty="solid")
  lines(tt,reet.K[,i],col=cols[i],lwd=1,lty="solid")
}

# plot 2
plot(0,-2,col="ivory",pch=17,xlim=c(600,900),ylim=c(0,600),xlab="T (°C)", ylab="REE (ppm)")
for(i in 1:3){
  lines(Temp,reet.M.part[,i],col=cols[i],lwd=2,lty="dashed")
  lines(Temp,reet.M.glob[,i],col=cols[i],lwd=2,lty="dotted")
  lines(Temp,lree.S[,i],col=cols[i],lwd=2,lty="solid")
  lines(Temp,reet.K[,i],col=cols[i],lwd=1,lty="solid")
}
