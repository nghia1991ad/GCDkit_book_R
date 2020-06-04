####################################################
#
# Part VI, Chapter 26: Worked example:
# Saint-Malo Complex progressive melting 
#
####################################################

# The functions from Part_VI_common_functions.r must be loaded beforehand:
source("Part_VI_common_functions.r") #or drag-and-drop the file onto the R-console

########## Definition, data loading ############
loadData("stmalo.data")
assignColVar("Melt_frac","reds")

################################################
# Section 26.2 Major and Trace Elements
################################################

########## Harker diagrams ########################
multiple("SiO2","Al2O3,Fe2O3,MgO,CaO,Na2O,K2O") # Fig. 26.3
multiple("SiO2","Rb,Sr,Zr,Ni,Cr,V")# Fig. 26.4
plate0YLim() # to set zeroes as minima to all y axes
plateCex(2)
plateCexLab(2)

################################################
# Section 26.3.1 Mode Evolution During melting
################################################

########## Mode changes during melting ############ 
mins <- c("Qtz","Kfs","Ab","An","Bt","Sil","Crd")
mm <- c(mins,"Melt")

# Function to compute the mode
melt.mode <- function(m0,stc,ff){
    m <- m0[mm]+ff/100*stc[mm]
    return(m)
}

## Mica-rich gneiss MRG
stc.tab <- read.table("stmalo_stc_mrg.data",sep="\t")
stc.tab <- as.matrix(stc.tab)

res <- stc.tab["m0",mm,drop=F]  # initialize res
for(i in 2:5){                  # corresponding to lines of stc.tab
    f.r <- seq(1,stc.tab[i,"Fmax"]-stc.tab[i-1,"Fmax"],1)
    stc <- stc.tab[i,mm]
    m0 <- res[nrow(res),mm]     # The last line of prev stage
    ee <- t(sapply(f.r,FUN=function(z){
        melt.mode(m0,stc,z)
    }))
    res <- rbind(res,ee)        # update res before the next step
}

Pl <- res[,"Ab"]+res[,"An"]
res <- cbind(res,Pl)
mode.mrg <- res

## Quartzo-feldspathic gneiss QFG
stc.tab <- read.table("stmalo_stc_qfg.data",sep="\t")
stc.tab <- as.matrix(stc.tab)

res <- stc.tab["m0",mm,drop=F]
for(i in 2:4){ #only 3 melting steps for QFG !
    f.r <- seq(1,stc.tab[i,"Fmax"]-stc.tab[i-1,"Fmax"],1)
    stc <- stc.tab[i,mm]
    m0 <- res[nrow(res),mm]
    ee <- t(sapply(f.r,FUN=function(z){
        melt.mode(m0,stc,z)
    }))
    res <- rbind(res,ee)
}

Pl <- res[,"Ab"]+res[,"An"]
res <- cbind(res,Pl)
mode.qfg <- res

# Fig. 26.5
windows()
par(mfrow=c(2,2))
plot(1,1,ylim=c(0,40),xlim=c(0,100),pch="",xlab="F",ylab="min. prop. in full system")
lines(mode.mrg[,"Melt"],mode.mrg[,"Qtz"],lwd=1.5,col="grey")
lines(mode.mrg[,"Melt"],mode.mrg[,"Kfs"],lwd=1.5,col="blue")
lines(mode.mrg[,"Melt"],mode.mrg[,"Bt"],lwd=1.5,col="orange")
lines(mode.mrg[,"Melt"],mode.mrg[,"Pl"],lwd=1.5,col="lightblue")
lines(mode.mrg[,"Melt"],mode.mrg[,"Crd"],lwd=1.5,col="green")

lines(mode.mrg[,"Melt"],mode.mrg[,"Qtz"],lwd=1.5,col="grey")
lines(mode.mrg[,"Melt"],mode.mrg[,"Kfs"],lwd=1.5,col="blue")
lines(mode.mrg[,"Melt"],mode.mrg[,"Bt"],lwd=1.5,col="orange")
lines(mode.mrg[,"Melt"],mode.mrg[,"Pl"],lwd=1.5,col="lightblue")
lines(mode.mrg[,"Melt"],mode.mrg[,"Crd"],lwd=1.5,col="green")

plot(1,1,ylim=c(0,40),xlim=c(0,100),pch="",xlab="F",ylab="min. prop. in full system")
lines(mode.qfg[,"Melt"],mode.qfg[,"Qtz"],lwd=1.5,col="grey")
lines(mode.qfg[,"Melt"],mode.qfg[,"Kfs"],lwd=1.5,col="blue")
lines(mode.qfg[,"Melt"],mode.qfg[,"Bt"],lwd=1.5,col="orange")
lines(mode.qfg[,"Melt"],mode.qfg[,"Pl"],lwd=1.5,col="lightblue")
lines(mode.qfg[,"Melt"],mode.qfg[,"Crd"],lwd=1.5,col="green")

plot(1,1,ylim=c(0,60),xlim=c(0,100),pch="",xlab="F",ylab="min. prop. in residue (m)")
lines(mode.mrg[,"Melt"],mode.mrg[,"Qtz"]/(100-mode.mrg[,"Melt"])*100,lwd=1.5,col="grey")
lines(mode.mrg[,"Melt"],mode.mrg[,"Kfs"]/(100-mode.mrg[,"Melt"])*100,lwd=1.5,col="blue")
lines(mode.mrg[,"Melt"],mode.mrg[,"Bt"]/(100-mode.mrg[,"Melt"])*100,lwd=1.5,col="orange")
lines(mode.mrg[,"Melt"],mode.mrg[,"Pl"]/(100-mode.mrg[,"Melt"])*100,lwd=1.5,col="lightblue")
lines(mode.mrg[,"Melt"],mode.mrg[,"Crd"]/(100-mode.mrg[,"Melt"])*100,lwd=1.5,col="green")

plot(1,1,ylim=c(0,60),xlim=c(0,100),pch="",xlab="F",ylab="min. prop. in residue (m)")
lines(mode.qfg[,"Melt"],mode.qfg[,"Qtz"]/(100-mode.qfg[,"Melt"])*100,lwd=1.5,col="grey")
lines(mode.qfg[,"Melt"],mode.qfg[,"Kfs"]/(100-mode.qfg[,"Melt"])*100,lwd=1.5,col="blue")
lines(mode.qfg[,"Melt"],mode.qfg[,"Bt"]/(100-mode.qfg[,"Melt"])*100,lwd=1.5,col="orange")
lines(mode.qfg[,"Melt"],mode.qfg[,"Pl"]/(100-mode.qfg[,"Melt"])*100,lwd=1.5,col="lightblue")
lines(mode.qfg[,"Melt"],mode.qfg[,"Crd"]/(100-mode.qfg[,"Melt"])*100,lwd=1.5,col="green")

###################################################
# Section 26.3.2 Major and Trace Elements
###################################################

# The following two lines are contributed by my cat walking on the keyboard
# Not sure what they mean, perhaps they make more sense that it seems?
# aaaaafgvgyyyyyyyyyluôç:!iuoi
# :yhp^mù$**

########## Mode changes during melting ############ 
mins <- c("Qtz","Kfs","Ab","An","Bt","Sil","Crd")
mm <- c(mins,"Melt")

# Function to compute the mode
melt.mode <- function(m0,stc,ff){
    m <- m0[mm]+ff/100*stc[mm]
    return(m)
}

## Mica-rich gneiss MRG
stc.tab <- read.table("stmalo_stc_mrg.data",sep="\t")
stc.tab <- as.matrix(stc.tab)

res <- stc.tab["m0",mm,drop=F]  # initialize res
for(i in 2:5){                  # corresponding to lines of stc.tab
    f.r <- seq(1,stc.tab[i,"Fmax"]-stc.tab[i-1,"Fmax"],1)
    stc <- stc.tab[i,mm]
    m0 <- res[nrow(res),mm]     # The last line of prev stage
    ee <- t(sapply(f.r,FUN=function(z){
        melt.mode(m0,stc,z)
    }))
    res <- rbind(res,ee)        # update res before the next step
}

Pl <- res[,"Ab"]+res[,"An"]
res <- cbind(res,Pl)
mode.mrg <- res

## Quartzo-feldspathic gneiss QFG
stc.tab <- read.table("stmalo_stc_qfg.data",sep="\t")
stc.tab <- as.matrix(stc.tab)

res <- stc.tab["m0",mm,drop=F]
for(i in 2:4){ #only 3 melting steps for QFG !
    f.r <- seq(1,stc.tab[i,"Fmax"]-stc.tab[i-1,"Fmax"],1)
    stc <- stc.tab[i,mm]
    m0 <- res[nrow(res),mm]
    ee <- t(sapply(f.r,FUN=function(z){
        melt.mode(m0,stc,z)
    }))
    res <- rbind(res,ee)
}

Pl <- res[,"Ab"]+res[,"An"]
res <- cbind(res,Pl)
mode.qfg <- res

########## Fwd model for major and traces ############
# Same function as Chap. 25
# If the two exercises are run sequentially, no need to redefine it !
fwd.mod <- function(c0.lab,m,ff,norm=F,eqn="FC"){ 
    min.set <- names(m)
    kd <- kd.tab[min.set,trc]
    c0t <- WR[c0.lab,trc]
    dd <- m%*%as.matrix(kd)
    cLc.t <- switch(eqn, 
                    FC=c0t*ff^(dd-1),
                    PM=c0t/(dd+ff*(1-dd)))
    
    mincomp <- min.tab[min.set,mjrs]
    c0m <- WR[c0.lab,mjrs]
    if(norm){
        c0 <- c0m/sum(c0m)*100 # Fixed c0m instead of c0?
        mincomp <- t(apply(mincomp,1,FUN=function(z) {z <- z/sum(z)*100; return(z)}))
    }
    cs <- m%*%as.matrix(mincomp)
    cLc.m <- (c0m-(1-ff)*cs)/ff
    
    cLc <- c(cLc.m,cLc.t)
    names(cLc) <- c(mjrs,trc)
    res <- list(cS=cs,cL=cLc,dd=dd)
    return(res)
}

min.tab <- read.table("stmalo_mins.data",sep="\t")
kd.tab <- read.table("stmalo_kd.data",sep="\t")
mjrs <- c("SiO2","Al2O3","Fe2O3","MgO","CaO","Na2O","K2O","TiO2")
trc <- c("Rb","Sr","Zr","Ni","Cr","V")

## Models
# Model for MRG
c0.lab <- "MRG"

melts.mrg <- t(apply(mode.mrg,1,function(z){
    m <- z[mins]/(100-z["Melt"])
    ff <- z["Melt"]/100
    ee <- fwd.mod(c0.lab,m,ff,eqn="PM")$cL
    return(ee)
}))

# Model for QFG
c0.lab  <-  "QFG"
melts.qfg <- t(apply(mode.qfg,1,function(z){
    m <- z[mins]/(100-z["Melt"])
    ff <- z["Melt"]/100
    ee <- fwd.mod(c0.lab,m,ff,eqn="PM")$cL
    return(ee)
}))

## Plotting
# Common function to plot one graph with the models on top
diag.seqm <- function(el,melts.mrg,melts.qfg,ymax=-1){
    if(ymax==-1){
        ymax <- range(melts.mrg[,el],melts.qfg[,el],WR[,el],na.rm=T,finite=T)[2]
    }
    binary("Melt_frac",el,new=F,xmin=0,xmax=1,ymax=ymax,ymin=0,xlab="F")
    figCex(2)
    figCexLab(1.5)
    points(1,WR["MRG",el],pch=15,cex=2,col="darkgreen")
    points(1,WR["QFG",el],pch=15,cex=2,col="darkblue")
    lines(mode.mrg[,"Melt"]/100,melts.mrg[,el],lwd=1.5,col="green")
    lines(mode.qfg[,"Melt"]/100,melts.qfg[,el],lwd=1.5,col="blue")
}

# Figure 26.6
y.axs <- c("Al2O3","Fe2O3","MgO","CaO","Na2O","K2O",trc)
multiplePerPage(12,nrow=4,ncol=3,title=NULL)
for(j in 1:12){
    screen(j)
    diag.seqm(y.axs[j],melts.mrg,melts.qfg)
}

###################################################
# Section 26.3.3 Zircon
###################################################

#min.tab <- read.table("stmalo_mins.data",sep="\t")
#kd.tab <- read.table("stmalo_kd.data",sep="\t")
mjrs <- c("SiO2","Al2O3","Fe2O3","MgO","CaO","Na2O","K2O","TiO2")
trc <- c("Rb","Sr","Zr","Ni","Cr","V")

########## Mode changes during melting ############ 
mins <- c("Qtz","Kfs","Ab","An","Bt","Sil","Crd")
mm <- c(mins,"Melt")

# Function to compute the mode
melt.mode <- function(m0,stc,ff){
    m <- m0[mm]+ff/100*stc[mm]
    return(m)
}

## Mica-rich gneiss MRG
stc.tab <- read.table("stmalo_stc_mrg.data",sep="\t")
stc.tab <- as.matrix(stc.tab)

res <- stc.tab["m0",mm,drop=F]  # initialize res
for(i in 2:5){                  # corresponding to lines of stc.tab
    f.r <- seq(1,stc.tab[i,"Fmax"]-stc.tab[i-1,"Fmax"],1)
    stc <- stc.tab[i,mm]
    m0 <- res[nrow(res),mm]     # The last line of prev stage
    ee <- t(sapply(f.r,FUN=function(z){
        melt.mode(m0,stc,z)
    }))
    res <- rbind(res,ee)        # update res before the next step
}

Pl <- res[,"Ab"]+res[,"An"]
res <- cbind(res,Pl)
mode.mrg <- res

## Quartzo-feldspathic gneiss QFG
stc.tab <- read.table("stmalo_stc_qfg.data",sep="\t")
stc.tab <- as.matrix(stc.tab)

res <- stc.tab["m0",mm,drop=F]
for(i in 2:4){ #only 3 melting steps for QFG !
    f.r <- seq(1,stc.tab[i,"Fmax"]-stc.tab[i-1,"Fmax"],1)
    stc <- stc.tab[i,mm]
    m0 <- res[nrow(res),mm]
    ee <- t(sapply(f.r,FUN=function(z){
        melt.mode(m0,stc,z)
    }))
    res <- rbind(res,ee)
}

Pl <- res[,"Ab"]+res[,"An"]
res <- cbind(res,Pl)
mode.qfg <- res

########## Fwd model for major and traces ############
# Same function as Chap. 25
# If the two exercises are run sequentially, no need to redefine it!
fwd.mod <- function(c0.lab,m,ff,norm=F,eqn="FC"){ 
    min.set <- names(m)
    kd <- kd.tab[min.set,trc]
    c0t <- WR[c0.lab,trc]
    dd <- m%*%as.matrix(kd)
    cLc.t <- switch(eqn, 
                    FC=c0t*ff^(dd-1),
                    PM=c0t/(dd+ff*(1-dd)))
    
    mincomp <- min.tab[min.set,mjrs]
    c0m <- WR[c0.lab,mjrs]
    if(norm){
        c0 <- c0m/sum(c0m)*100 # Fixed c0m instead of c0?
        mincomp <- t(apply(mincomp,1,FUN=function(z) {z <- z/sum(z)*100; return(z)}))
    }
    cs <- m%*%as.matrix(mincomp)
    cLc.m <- (c0m-(1-ff)*cs)/ff
    
    cLc <- c(cLc.m,cLc.t)
    names(cLc) <- c(mjrs,trc)
    res <- list(cS=cs,cL=cLc,dd=dd)
    return(res)
}


########## Tweaking the model for Zr ############  
mins2 <- c(mins,"Zrn")

melts.with.zrn<-function(c0.lab,litho.mode,m.zrn){
    # Adding zircon 
    Zrn<- m.zrn*(100-litho.mode[,"Melt"])/100
    corrected.mode<-cbind(litho.mode,Zrn)
    # renormalizing to Sum(m) = 1 will be done within fwd.mod so no need to worry!
    corrected.melt <- apply(corrected.mode,1,function(z){
        m <- z[mins2]/(100-z["Melt"])
        ff <- z["Melt"]/100
        ee <- fwd.mod(c0.lab,m,ff,eqn="PM")$cL
        return(ee)
    })
    corrected.melt <- t(corrected.melt)
    return(corrected.melt)
}

# Calculation
zr.props <- c(0.01,0.02,0.05,0.1,0.5,1)

# MRG
zr.mrg<-sapply(zr.props,function(i){
    z <- melts.with.zrn("MRG",mode.mrg,i)
    return(z[,"Zr"])
})
colnames(zr.mrg) <- zr.props

# QFG
zr.qfg<-sapply(zr.props,function(i){
    z <- melts.with.zrn("QFG",mode.mrg,i)
    return(z[,"Zr"])
})
colnames(zr.qfg) <- zr.props

## Fig. 26.7
multiplePerPage(6,nrow=2,ncol=3,title=NULL)
for(i in 1:6){
    zr.added <- zr.props[i]
    screen(i)
    binary("Melt_frac*100","Zr",xmin=0,xmax=100,ymin=0,ymax=400,xlab="F %",main=paste("Zrn prop. =",zr.added,"wt. %"),new=F)
    figCex(2)
    figCexLab(1.5)
    
    points(100,WR["MRG","Zr"],pch=15,cex=2,col="darkgreen")
    points(100,WR["QFG","Zr"],pch=15,cex=2,col="darkblue")    
    
    lines(mode.mrg[,"Melt"],zr.mrg[,i],lwd=1.5,col="green")
    lines(mode.qfg[,"Melt"],zr.qfg[,i],lwd=1.5,col="blue")
}
