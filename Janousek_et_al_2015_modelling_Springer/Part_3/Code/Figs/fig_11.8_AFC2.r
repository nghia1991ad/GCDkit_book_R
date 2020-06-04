# Normalized concentration
r<-0.2    # ca/c0

windows(width=10,height=10,pointsize=14)
par(mfrow=c(2,2))
a<-c(0,1,10,100)
col<-selectPalette(5,"reds")
for (D in c(0,1,2,10)){
    plot(1,1,xlim=c(1,0),ylim=c(0.001,1000),xlab="F",ylab=expression(C[0]/C[L]),log="y",xaxs="i",yaxs="i",lab=c(10,5,0),asp=1,main=paste("D = ",D),type="n")
    abline(h=c(0.01,0.1,1,10,100),col="gray")
    
    for (i in 1:length(a)){
        z<-1-D/(1-r)
        F<-seq(0,1,by=0.01)
        Y<-F^-z+r/(z*(r-1))*a[i]*(1-F^-z)
    if(i==1){
        points(F,Y,type="l",lty="dashed")
        }else{
        points(F,Y,type="l",col=col[i])
        }
        
    }

}
