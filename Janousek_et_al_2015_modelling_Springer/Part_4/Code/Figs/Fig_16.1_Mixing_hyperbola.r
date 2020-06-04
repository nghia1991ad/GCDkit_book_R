a<-100
b<-50
xmin<-100
xmax<-600

windows(width=10,height=6)
par(mfrow=c(1,2))
curve(a/x+b,xlab=expression(C[M]),ylab=expression(I[M]),from=xmin,to=xmax)
curve(a*x+b,xlab=expression(1/C[M]),ylab=expression(I[M]),from=1/xmax,to=1/xmin)
