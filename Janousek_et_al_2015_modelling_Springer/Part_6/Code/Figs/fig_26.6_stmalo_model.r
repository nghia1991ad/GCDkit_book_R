
# The functions from Part_VI_common_functions.r must be loaded beforehand:
source("Part_VI_common_functions.r") #or drag-and-drop the file onto the R-console

########## Definition, data loading ############
loadData("stmalo.data")
assignColVar("Melt_frac","reds")

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

