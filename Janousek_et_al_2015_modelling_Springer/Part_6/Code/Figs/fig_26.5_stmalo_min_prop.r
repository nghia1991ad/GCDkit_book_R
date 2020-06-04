
# The functions from Part_VI_common_functions.r must be loaded beforehand:
source("Part_VI_common_functions.r") #or drag-and-drop the file onto the R-console

########## Definition, data loading ############
loadData("stmalo.data")
assignColVar("Melt_frac","reds")

####### Mode changes during melting ############ 
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
