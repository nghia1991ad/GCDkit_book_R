# In order to exactly reproduce the book figure, load the saved dataset :

loadData("noisy.data")
# Code generating the artificial data in the file 'noisy.data' can be found in partV_generate_synthetic_data.r


# Calculate different models
#############################

# Aux function, transforms the col names "X_corr" in proper element name "X"
correct.mod.table<-function(mt){
    extract<-c(mjrs,paste(trc,"corr",sep="_"))
    mt<-mt[,extract]
    colnames(mt)<-c(mjrs,trc)
    return(mt)
}

# mixing line (for maj and traces)
xB<-seq(0,1,0.02)
    foo<-sapply(xB,function(i){
    mt<-correct.mod.table(fwd.mod)
    return(mt[1,]*i+mt[nrow(mt),]*(1-i))
}
)
mix<-t(foo)
mix<-cbind(xB,mix)

# Batch line (for maj and traces, maj from mass balance)
gg<-seq(1,0,-0.01)
foo<-sapply(gg,function(i){
    mt<-correct.mod.table(fwd.mod)
    b.trc<-mt[1,trc]/(D+i*(1-D))
    names(b.trc)<-trc
    b.maj<-(c0[mjrs]-(1-i)*cs[1,mjrs])/i
    b.maj<-b.maj/sum(b.maj)*tot.anh(i)
    return(c(b.maj,b.trc))
}
)
batch<-t(foo)
batch<-cbind(gg,batch)


# Generate book figures
#########################

# fig. 24.2

windows(13,15)
par(mfrow=c(3,3))

# Natural scale
plot(WR[,"Rb"],WR[,"Ba"],xlab="Rb",ylab="Ba",pch=15)
lines(fwd.mod[,"Rb_corr"],fwd.mod[,"Ba_corr"],col="red",lwd=2)
lines(mix[,"Rb"],mix[,"Ba"],col="blue")
lines(batch[,"Rb"],batch[,"Ba"],col="green")

plot(WR[,"V"],WR[,"Ni"],xlab="V",ylab="Ni",pch=15)
lines(fwd.mod[,"V_corr"],fwd.mod[,"Ni_corr"],col="red",lwd=2)
lines(mix[,"V"],mix[,"Ni"],col="blue")
lines(batch[,"V"],batch[,"Ni"],col="green")

plot(WR[,"Rb"],WR[,"Ni"],xlab="Rb",ylab="Ni",pch=15)
lines(fwd.mod[,"Rb_corr"],fwd.mod[,"Ni_corr"],col="red",lwd=2)
lines(mix[,"Rb"],mix[,"Ni"],col="blue")
lines(batch[,"Rb"],batch[,"Ni"],col="green")

# Log scale
plot(WR[,"Rb"],WR[,"Ba"],xlab="Rb",ylab="Ba",pch=15,log="xy")
lines(fwd.mod[,"Rb_corr"],fwd.mod[,"Ba_corr"],col="red",lwd=2)
lines(mix[,"Rb"],mix[,"Ba"],col="blue")
lines(batch[,"Rb"],batch[,"Ba"],col="green")

plot(WR[,"V"],WR[,"Ni"],xlab="V",ylab="Ni",pch=15,log="xy")
lines(fwd.mod[,"V_corr"],fwd.mod[,"Ni_corr"],col="red",lwd=2)
lines(mix[,"V"],mix[,"Ni"],col="blue")
lines(batch[,"V"],batch[,"Ni"],col="green")

plot(WR[,"Rb"],WR[,"Ni"],xlab="Rb",ylab="Ni",pch=15,log="xy")
lines(fwd.mod[,"Rb_corr"],fwd.mod[,"Ni_corr"],col="red",lwd=2)
lines(mix[,"Rb"],mix[,"Ni"],col="blue")
lines(batch[,"Rb"],batch[,"Ni"],col="green")

# Ratios
plot(WR[,"Rb"]/WR[,"Ni"],1/WR[,"Ni"],xlab="Rb/Ni",ylab="1/Ni",pch=15)
lines(fwd.mod[,"Rb_corr"]/fwd.mod[,"Ni_corr"],1/fwd.mod[,"Ni_corr"],col="red",lwd=2)
lines(mix[,"Rb"]/mix[,"Ni"],1/mix[,"Ni"],col="blue")
lines(batch[,"Rb"]/batch[,"Ni"],1/batch[,"Ni"],col="green",lty="dotted")
lines(batch[1:55,"Rb"]/batch[1:55,"Ni"],1/batch[1:55,"Ni"],col="green")

lq<-lsfit(WR[,"Rb"]/WR[,"Ni"],1/WR[,"Ni"])
ls.print(lq)
abline(lq,lty="dashed",add=T)


plot(WR[,"Rb"]/WR[,"Ni"],WR[,"Ni"],xlab="Rb/Ni",ylab="Ni",pch=15)
lines(fwd.mod[,"Rb_corr"]/fwd.mod[,"Ni_corr"],fwd.mod[,"Ni_corr"],col="red",lwd=2)
lines(mix[,"Rb"]/mix[,"Ni"],mix[,"Ni"],col="blue")
lines(batch[,"Rb"]/batch[,"Ni"],batch[,"Ni"],col="green",lty="dotted")
lines(batch[1:55,"Rb"]/batch[1:55,"Ni"],batch[1:55,"Ni"],col="green")

plot(WR[,"Rb"]/WR[,"Ni"],WR[,"Rb"],xlab="Rb/Ni",ylab="Rb",pch=15)
lines(fwd.mod[,"Rb_corr"]/fwd.mod[,"Ni_corr"],fwd.mod[,"Rb_corr"],col="red",lwd=2)
lines(mix[,"Rb"]/mix[,"Ni"],mix[,"Rb"],col="blue")
lines(batch[,"Rb"]/batch[,"Ni"],batch[,"Rb"],col="green",lty="dotted")
lines(batch[1:55,"Rb"]/batch[1:55,"Ni"],batch[1:55,"Rb"],col="green")

lq<-lsfit(WR[,"Rb"]/WR[,"Ni"],WR[,"Rb"])
ls.print(lq)
abline(lq,lty="dashed",add=T)
