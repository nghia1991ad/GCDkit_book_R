windows()
usr <- par("usr")
par(mai = c(0.7, 0.7, 0.7, 0.7))
par(omi = c(0, 0, 0, 0))
par(mfrow=c(2,2))
par(pty="s")

ca<-100
ia<-0.720
c0<-400
i0<-0.703
srlab<- expression(""^87*Sr/""^86*Sr)
col<-selectPalette(7,"terrain.colors")
    
# Calculates concentration of an element after AFC
AFCelem<-function(c0,ca,D,r,F=seq(0,1,by=.001)){
        z<--(r+D-1)/(r-1)
        if (r>1){z<-abs(z)}
        f<-F^z
        X<-c0*f+r/(r-1+D)*ca*(1-f)
        return(X)
}

# Calculates isotopic composition after AFC 
AFCiso<-function(c0,cm,i0,ia,D,r,F=seq(0,1,by=.001)){
        z<--(r+D-1)/(r-1)
        if (r>1){z<-abs(z)}
        f<-F^z
        Y<-i0+(ia-i0)*(1-c0/cm*f)
        return(Y)
}

# Get r, and D ready, cycle
r<-c(0.2,0.5,0.8,1.5)
D<-c(0.01,0.1,0.5,0.7,0.9,1,2)

for (i in 1:length(r)){
    plot(c0,i0,xlim=c(0,700),ylim=c(0.700,ia+0.001),xaxs="i",yaxs="i",xlab="Sr (ppm)",ylab=srlab)
    for (j in 1:length(D)){
        X<-AFCelem(c0,ca,D[j],r[i])     
        Y<-AFCiso(c0,X,i0,ia,D[j],r[i])
        points(X,Y,type="l",col=col[j],lwd=1.5)
    }
    points(ca,ia,pch=19)
    text(ca,ia,"Assimilant",cex=0.8,pos=1)
    points(c0,i0,pch=19)
    text(c0,i0,"Initial liquid",cex=0.8,pos=1)
    
}
par(usr)
