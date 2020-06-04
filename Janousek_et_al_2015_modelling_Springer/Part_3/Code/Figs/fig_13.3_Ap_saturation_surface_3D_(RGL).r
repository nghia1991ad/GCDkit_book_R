##### Fig. 13.3 Apatite sat  3D (using rgl)

require(rgl)||stop("Missing package rgl!")

ACNK<-seq(1,1.5,0.05)
Temp<-seq(600,900,10)

Si<-0.7
Press<-5

cols<-c("pink","red","brown")

p.wp<-function(a,tt){
  if(tt==750){
    return(-3.4+3.1*a)
  }else{
    return(NA)
  }
}

p.h<-function(a,tt){
    A<-8400+(Si-0.5)*26400
    B<-3.1+12.4*(Si-0.5) 
    D.HW<-exp(A/(tt+273)-B)
    return(42/D.HW)
}

p.p<-function(a,tt){
    C<--5900
    D<--3.22*Si+9.31  
    return(p.h(a=a,tt=tt)+(a-1)*exp(C/(tt+273)+D)) 
}

p.b<-function(a,tt){
    E<-(a-1)*6429
    return(p.h(a=,tt=tt)*exp(E/(tt-273.15)))
}

P2O5.HW<-outer(ACNK,Temp,FUN="p.h")
P2O5.PV<-outer(ACNK,Temp,FUN="p.p")
P2O5.Bea<-outer(ACNK,Temp,FUN="p.b")

P2O5.Bea[P2O5.Bea>6]<-NA
projM<-persp3d(ACNK,Temp,P2O5.Bea,
       ticktype="detailed",
       shade=0.5,col="ivory",
       border="darkgrey",
       ylab="T (°C)",
       xlab="A/CNK",
       zlim=c(0,6),
       zlab="P2O5 (wt%)",
       theta=-30,phi=20)

persp3d(ACNK,Temp,P2O5.PV,shade=0.5,col="lightblue",add=T)

persp3d(ACNK,Temp,P2O5.HW,shade=0.5,col="darkred",add=T)              

# Adjust manually angle of view to reproduce Fig. 13.3 !
