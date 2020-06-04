# Monte-Carlo approach

# The functions from Part_V_common_functions.r must be loaded beforehand:
# drag-and-drop the file onto the R-console or run source("Part_V_common_functions.r")


# Moreover, you need to run the complete code to the Chapter 25, stored in \Complete_code\chap_25.r!

fwd.maj.mc<-function(c0.lab,cL.lab,min.set,ff,m,norm=F){
  names(m)<-min.set
  rr<-fwd.mod(c0.lab=c0.lab,m=m,ff=ff,norm=T,eqn="FC")$cL[mjrs]-WR[cL.lab,mjrs]
  return(sum(rr^2))
}
  
montecarlo<-function(nb.mod,min.set,guess,fuzzy,c0.lab,cL.lab,thres,keep.all=F){
  n.ex<-0
  res<-NULL
  ll<-length(min.set)+1
  nm<-c(min.set,"F")
  while(n.ex<nb.mod){
    m.min<-guess*(1-fuzzy)
    m.max<-guess*(1+fuzzy)
    m0<-runif(ll,min=m.min,max=m.max)
    m1<-m0[1:ll-1]/sum(m0[1:ll-1])
    mm<-c(m1,m0[ll])
    foo<-fwd.maj.mc(c0.lab,cL.lab,min.set,mm[ll],mm[1:ll-1])
    cat("Residue: ",foo,"...")
    if(keep.all){
         n.ex<-n.ex+1
         mm<-c(mm,foo)
         res<-rbind(res,mm)  
         cat("\n")
    }else{
    if(foo<thres){
         n.ex<-n.ex+1
         mm<-c(mm,foo)
         res<-rbind(res,mm)
         cat("SUCCESS !\n")
      }else{cat("rejected\n")}
    }
  }
  colnames(res)<-c(nm,"R2")
  return(res)
}

# Generate 1000 models
ee<-montecarlo(nb.mod=1000,min.set=min.set.1cpx,
guess=c(rev.maj("ATAC-4","ATAC-58",min.set.1cpx)$m,
        rev.maj("ATAC-4","ATAC-58",min.set.1cpx)$ff),
fuzzy=0.5,
c0.lab="ATAC-4",cL.lab="ATAC-58",thres=0.25,keep.all=T)
# Takes a bit of time to run, depending on the system !

# Fig. 25.8
windows(h=4,w=9) 
par(mfrow=c(1,3))
  plot(ee[,"F"],ee[,"R2"],log="y",xlab="melt amount (F)",ylab=expression(Sigma*R^2),cex.lab=1.5,ylim=c(mod1cpx$r.sq*0.9,max(ee[,"R2"])),pch=20,cex=1,col="brown")
  abline(v=mod1cpx$ff,col="grey")
  abline(h=mod1cpx$r.sq,col="grey")
    points(mod1cpx$ff,mod1cpx$r.sq,pch=15,cex=2,col="red")
             
   plot(ee[,"An"],ee[,"R2"],log="y",xlab="Anorthite prop.",ylab=expression(Sigma*R^2),cex.lab=1.5,ylim=c(mod1cpx$r.sq*0.9,max(ee[,"R2"])),pch=20,cex=1,col="darkgreen")
  abline(v=mod1cpx$m["An"],col="grey")
  abline(h=mod1cpx$r.sq,col="grey")
    points(mod1cpx$m["An"],mod1cpx$r.sq,pch=15,cex=2,col="green")   
   
   plot(ee[,"Mt"],ee[,"R2"],log="y",xlab="Magnetite prop.",ylab=expression(Sigma*R^2),cex.lab=1.5,ylim=c(mod1cpx$r.sq*0.9,max(ee[,"R2"])),pch=20,cex=1,col="darkblue")
  abline(v=mod1cpx$m["Mt"],col="grey")
  abline(h=mod1cpx$r.sq,col="grey")
    points(mod1cpx$m["Mt"],mod1cpx$r.sq,pch=15,cex=2,col="blue")       
