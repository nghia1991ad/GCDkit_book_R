## Same definitions as fig. 13.1
Mstp<-0.05
M1<-seq(0.85,1.7,Mstp)
M2<-seq(1.75,2.05,Mstp)
M<-c(M1,M2)

Temp<-seq(600,1000,20)
Zr.sat<-outer(M,Temp,function(mi,ti) 497644/(exp(-3.8-0.85*(mi-1)+12900/(ti+273))))

## Not used, generates M-T plot
contour(M,Temp,Zr.sat,
       ylab="T (°C)",
       xlab="M",
       levels=c(20,50,100,200,500,1000,2000)
)
       
## Figure 13.4       
 # flatten Zr.sat matrix (to xyz triplets)
 Zr.xyz<-c(0,0,0)
 names(Zr.xyz)<-c("M","Temp","Zr.sat")      
 for(i in 1:length(M)){
    for(j in 1:length(Temp)){
        qq<-c(M[i],Temp[j],Zr.sat[i,j])
        Zr.xyz<-rbind(Zr.xyz,qq)
    }
 }      
 Zr.xyz<-Zr.xyz[-1,]
 
 # plot a template (B&W)
 plot(0,-2,col="ivory",pch=17,xlim=c(-2.05,-0.85),ylim=c(0,1500))
 for(tt in Temp){
    foo<-Zr.xyz[Zr.xyz[,"Temp"]==tt,]
    lines(-foo[,"M"],foo[,"Zr.sat"])
 }
 
 # (lots of) further graphic edits required to arrive to fig. 13.4 as printed !
