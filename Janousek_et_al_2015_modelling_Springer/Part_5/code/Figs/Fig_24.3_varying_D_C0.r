# Must run file partV_generate_synthetic_data.r first !
# In order to exactly reproduce the book figure, load the saved dataset :

loadData("noisy.data")


# Fuzzy curves -- jittering c0 and D
####################################

#Create jittered data
jittered<-function(el,d.scatt=1,c.scatt=1){
    rand.D<-runif(1,min=1/d.scatt,max=d.scatt) 
    rand.c<-runif(1,min=1/c.scatt,max=c.scatt)
    d.j<-rand.D*D[1,el]
    c0.j<-rand.c*c0[el]
    vals<-c0.j*ff^(d.j-1)
    return(list(c0=c0.j,data=vals,rand=rand.D))
}


# Plot a bunch of curves
plot.bunch<-function(ncrv=50,d.scatt=1,c.scatt=1,col="grey",lt=1,pts=F){
    for(i in 1:ncrv){
        x.jit<-jittered(x.el,d.scatt=d.scatt,c.scatt=c.scatt)
        x.data<-x.jit$data
        x.c0<-x.jit$c0
        y1.jit<-jittered(y1.el,d.scatt=d.scatt,c.scatt=c.scatt)
            y.data<-y1.jit$data
            y.c0<-y1.jit$c0
        lines(x.data,
              y.data,
              lty="solid",col=col,lwd=lt)
          if(pts){
            points(x.c0,y.c0,pch=20,col=col)
           }
    }
}


## Figure on Kd vs C0
# Fig. 24.3

# Elements on the axes
x.el<-"Rb"
y1.el<-"Ni"
 
# Start the plot
windows(width=12,height=6)  # Open an empty window of correct size
par(mfrow=c(1,2))   # Split the plotting window into two
 
## Plot 1 - changing D
# Create an empty plot (type="n") with an appropriate range
plot(1, type="n", xaxs="i", yaxs="i", xlim=c(20,65),
ylim=c(0,55), xlab=x.el, ylab=y1.el)
  
# Light curves, D varies by a factor 2  
ncrv<-200
plot.bunch(ncrv,d.scatt=2,c.scatt=1,col="pink",pts=F)

# Dark curves, D varies by 20%
ncrv<-50
plot.bunch(ncrv,d.scatt=1.2,c.scatt=1,col="darkred",pts=F)

# Reference curve and C0
lines(fwd.mod[,paste(x.el,"corr",sep="_")],fwd.mod[,paste(y1.el,"corr",sep="_")],col="red",lwd=2)
points(c0[x.el],c0[y1.el],cex=2,pch=8,col="red")
  
# Add "real" data
points(WR[,x.el],WR[,y1.el],pch=15,col="black")
 
## Plot 2 - changing C0
# Create an empty plot (type="n") with an appropriate range
plot(1, type="n", xaxs="i", yaxs="i", xlim=c(20,65),
ylim=c(0,55), xlab=x.el, ylab=y1.el)
  
# Light curves, D varies by a factor 2  
ncrv<-200
plot.bunch(ncrv,d.scatt=1,c.scatt=2,col="pink",pts=T)

# Dark curves, D varies by 20%
ncrv<-50
plot.bunch(ncrv,d.scatt=1,c.scatt=1.2,col="darkred",pts=T)

# Reference curve and C0
lines(fwd.mod[,paste(x.el,"corr",sep="_")],fwd.mod[,paste(y1.el,"corr",sep="_")],col="red",lwd=2)
points(c0[x.el],c0[y1.el],cex=2,pch=8,col="red")
  
# Add "real" data
points(WR[,x.el],WR[,y1.el],pch=15,col="black") 
 
