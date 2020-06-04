D<-c(0.01,0.5,2,5)
cols<-c("darkblue","lightblue","pink","darkred")

# Choice of equations
models<-list(
BatchLiq="1/(x+D[i]*(1-x))",
BatchSolid="D[i]/(x+D[i]*(1-x))",

RayleighLiq="x^(D[i]-1)",
RayleighAccumSolid="(1-x^D[i])/(1-x)",
RayleighInstSolid="D[i]*x^(D[i]-1)",

FracMeltInstLiq="(1/D[i])*(1-x)^((1/D[i])-1)",
FracMeltAccumLiq="(1/x)*(1-(1-x)^(1/D[i]))",
FracMeltSolid="(1-x)^(1/D[i]-1)"
)

compPlot<-function(reverse=F,legOfst=0,toplot=toplot,stroke=stroke){
names(stroke)<-toplot

# annotate is a GCDkit function dealing with subscripts, not plain R
# We create an empty plot (type="n"), with the right range (xlim, ylim), but no vertical axis (yaxt="n")
if(reverse){
xx<-c(1,0)
legDir<--1
}else{
xx<-c(0,1)
legDir<-1
}

plot(1, type="n", xaxs="i", yaxs="i", yaxt="n", xlim=xx, ylim=c(0.001,100), log="y", xlab="F", ylab=annotate("C[L]/C[0]"))

# Now we build a custom y-axis
yrange<-seq(-3,2,1)
yat<-10^yrange
axis(side=2,at=yat,labels=yat)

for (i in seq(1,length(D))){
# For each value of D, defined as a vector, we plot the required curves and label them
for (eq in toplot){
  ff<-as.function(alist(x=,eval(parse(text=models[[eq]]))))
  curve(ff,from=0,to=1,lty=stroke[eq],col=cols[i],add=TRUE)
}

#yloc<-1/(0.1+D[i]*(1-0.1))+yofst[i]
xloc<-xx[1]+legDir*(legOfst+0.01)
yloc<-10^(2.0-0.2*i)
text(xloc,yloc,pos=4,labels=paste("D=",D[i],sep=""),cex=1.2,col=cols[i])

}
#end for

for (j in seq(1,length(toplot))){
xshift<-xx[1]+legDir*(legOfst)
xloc<-xshift+c(0.25,0.4,0.42)*legDir
yloc<-10^(1.8-0.2*j) 
lines(c(xloc[1],xloc[2]),c(yloc,yloc),
  lty=stroke[j],col="black")
text(xloc[3],yloc,pos=4,
  labels=toplot[j],cex=1,col="black")  
}
    
abline(h=1)

}

# fig. 11.4
windows(11,6)
par(mfrow=c(1,2))
# plot 1
toplot<-c("BatchLiq","FracMeltInstLiq","FracMeltAccumLiq")
stroke<-c("dashed","solid","dotdash")
compPlot(reverse=F,legOfst=0.2,toplot=toplot,stroke=stroke)

# plot 2
toplot<-c("BatchSolid","FracMeltSolid")
stroke<-c("dashed","solid")
compPlot(reverse=F,legOfst=0.2,toplot=toplot,stroke=stroke)
