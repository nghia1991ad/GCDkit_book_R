###Figure 13.1 Zircon sat / 3D

require(grDevices)||stop("Missing package grDevices!")
 
###Zircon sat
Mstp<-0.05
M1<-seq(0.85,1.7,Mstp)
M2<-seq(1.75,2.05,Mstp)
M<-c(M1,M2)

Temp<-seq(600,1000,20)
Zr.sat<-outer(M,Temp,function(mi,ti) 497644/(exp(-3.8-0.85*(mi-1)+12900/(ti+273))))

# 3D plot
scol1<-rep("grey",length(M1))
scol2<-rep("white",length(M2)-1)
scol<-c(scol1,scol2)

projM<-persp(M,Temp,Zr.sat,
       ticktype="detailed",
       shade=0.5,col=scol,
       border="darkgrey",
       ylab="T (°C)",
       xlab="M",
       zlim=c(0,1000),
       zlab="Zr (ppm)",
       theta=40,phi=20)
# m-lines
drawat<-c(2,1.5,1.3)
for(i in drawat){
  idx<-which(M==i)     
  xx<-rep(M[idx],length(T))
  yy<-Temp
  zz<-Zr.sat[idx,]      
  lines(trans3d(xx,yy,zz,projM),lwd=2,col="darkred")
}
