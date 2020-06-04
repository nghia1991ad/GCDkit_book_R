windows()
i0 <- 0.704; c0 <- 150                          # the initial magma
ia <- 0.713; ca <- 500                          # the assimilant
dd <- 3.5                                       # bulk distribution coefficient for Sr
srlab <- expression(""^87*Sr/""^86*Sr)

plot(1/c0,i0,xlim=c(0,0.07),ylim=c(0.7035,0.7135),xlab="1/Sr (ppm)",ylab=srlab,type="n") 
                                                #draw empty plot
ee <- c(0,0.05,0.1,0.2,0.5)                     #vector of r parame-ters
for (r in ee){                                  #repeat for all r
    z <- (r+dd-1)/(r-1)
    ff <- seq(0.4,1,by=0.1)
    x <- c0*ff^-z+r/(z*(r-1))*ca*(1-ff^-z)      #Sr [Eq. (11.34)]
    y <- i0+(ia-i0)*(1-c0/x*ff^-z)              #87Sr/86Sr[Eq.(16.16))]
    points(1/x,y,type="b",pch=19)               #plot as a curve
    text(1/x[1],y[1],pos=3,paste("r =",r),cex=0.8)
}

# Plot end members
points(1/ca,ia,pch=19,cex=1.5,col="darkred")    #Contaminant
text(1/ca,ia,pos=2,"Contaminant",cex=0.8,srt=90,adj=1)
text(1/ca,ia,pos=3,expression(r==+infinity),cex=0.8)

points(1/c0,i0,pch=19,cex=1.5,col="darkblue")   #Initial magma
text(1/c0,i0,pos=1,"Initial magma",cex=0.8)
lines(c(1/c0,1/ca),c(i0,ia),lty="dashed",lwd=2,col="blue")
