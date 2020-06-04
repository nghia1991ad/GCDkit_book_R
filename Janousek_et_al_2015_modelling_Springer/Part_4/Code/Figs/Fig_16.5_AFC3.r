# Initial parameters
c0x<-600
cax<-600
x0<-0.703
xa<-0.715

c0y<-60
cay<-60
y0<-0.513
ya<-0.511

srlab<-expression(""^87*Sr/""^86*Sr)
ndlab<-expression(""^143*Nd/""^144*Nd)
    
# Calculates concentration of an element after AFC
AFCelem<-function(c0,ca,D,r,F=seq(0.01,1,by=.001)){
        z<--(r+D-1)/(r-1)
        if (r>1){z<-abs(z)}
        f<-F^z
        X<-c0*f+r/(r-1+D)*ca*(1-f)
        return(X)
}

# Calculates isotopic composition after AFC
AFCiso<-function(c0,cm,i0,ia,D,r,F=seq(0.01,1,by=.001)){
        z<--(r+D-1)/(r-1)
        if (r>1){z<-abs(z)}
        f<-F^z
        Y<-i0+(ia-i0)*(1-c0/cm*f)
        return(Y)
}

DSr<-c(0.01,2)
DNd<-c(2,0.01)
r<-c(0,0.1,0.2,0.5,1.5)

plot(0,0,xlim=c(0.702,0.716),ylim=c(0.5105,0.5135),xaxs="i",yaxs="i",xlab=srlab,ylab=ndlab,type="n")
col<-selectPalette(length(r)+1,"terrain.colors")

for(i in 1:length(r)){
    X<-AFCelem(c0x,cax,DSr[1],r[i])
    XX<-AFCiso(c0x,X,x0,xa,DSr[1],r[i])
    Y<-AFCelem(c0y,cay,DNd[1],r[i])
    YY<-AFCiso(c0y,Y,y0,ya,DNd[1],r[i])
    points(XX,YY,type="l",col=col[i])

    X<-AFCelem(c0x,cax,DSr[2],r[i])
    XX<-AFCiso(c0x,X,x0,xa,DSr[2],r[i])
    Y<-AFCelem(c0y,cay,DNd[2],r[i])
    YY<-AFCiso(c0y,Y,y0,ya,DNd[2],r[i])
    points(XX,YY,type="l",lty="dashed",col=col[i])
}

points(xa,ya,pch=19)
text(xa,ya,"Assimilant",cex=0.8,pos=1)

points(x0,y0,pch=19)
text(x0,y0,"Initial liquid",cex=0.8,pos=3)
