# Input parameters
x1<-0.703
y1<-0.513
x2<-0.712
y2<-0.511

# Plot
plot(0,0,xlim=c(0.7025,0.7125),ylim=c(0.5108,0.5132),type="n",xlab=expression(""^87*Sr/""^86*Sr),ylab=expression(""^143*Nd/""^144*Nd))

alpha<-c(10,2,1,0.5,0.1)
Sr1<-700
Sr2<-50
Nd1<-10
Nd2<-Sr2*Nd1/Sr1/alpha

# Parameters to the hyperbola
A<-y2*Nd2*Sr1-y1*Nd1*Sr2
B<-Nd1*Sr2-Nd2*Sr1
C<-x1*Nd2*Sr1-x2*Nd1*Sr2
D<-y1*x2*Nd1*Sr2-y2*x1*Nd2*Sr1

x<-seq(x1,x2,length=11)
col<-selectPalette(length(alpha),"reds")

# Hyperbola equation
ee<-lapply(1:length(alpha),function(i){
    eq<-eval(parse(text=paste("function(x)(",-D[i],"-",A[i],"*x)/(",B[i],"*x+",C[i],")",sep="")))
    print(eq)
    curve(eq,from=x1,to=x2,lwd=1,add=TRUE,col=col[i])
    y<-eq(x)
    points(x,y,pch=16,col=col[i])
    text(x[5],y[5]+0.0001,alpha[i],col=col[i])
    return(y)
})

#Asymptotes and curvature
x0<-(x1-alpha*x2)/(1-alpha)
y0<-(y2-alpha*y1)/(1-alpha)
q<-x0*y0+(alpha*x2*y1-x1*y2)/(1-alpha)

#End members
text(x1,y1,"1",pos=2)
text(x2,y2,"2",pos=4)
