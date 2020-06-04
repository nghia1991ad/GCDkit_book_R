# The functions from Part_VI_common_functions.r must be loaded beforehand:

source("Part_VI_common_functions.r") #or drag-and-drop the file onto the R-console

########## Definition, data loading ############
loadData("atacazo.data")
min.tab <- read.table("atacazo_mins.data",sep="\t")
kd.tab <- read.table("atacazo_kd.data",sep="\t")
mjrs <- c("SiO2","Al2O3","Fe2O3","MgO","CaO","Na2O","K2O","TiO2")
trc <- c("La","Ce","Nd","Sm","Eu","Gd","Dy","Er","Yb","Rb","Ba","Th","Nb","Sr","Zr","Y","V","Cr","Ni")


########## Stage 1 - ATAC-4 to ATAC-58 ############
c0.lab <- "ATAC-4"
cL.lab <- "ATAC-58"


#### Model with amp
min.set.1amp <- c("Opx","Ab","An","Rt","Mt","Amp")
mod1amp <- rev.maj(c0.lab, cL.lab,min.set.1amp,norm=T)

cL1amp <- fwd.mod(c0.lab,mod1amp$m,mod1amp$ff)$cL[trc]

# Adding zircon
zr.added <- 0.008/100 
ee <- c(mod1amp$m,zr.added)
min.prop.1ampzrc <- ee/sum(ee)
names(min.prop.1ampzrc)<- c(min.set.1amp,"Zrn")
cL1ampzrc <- fwd.mod(c0.lab,min.prop.1ampzrc,mod1amp$ff)$cL[trc]
foo <- rbind(WR[c0.lab,trc],WR[cL.lab,trc],cL1amp[trc],cL1ampzrc[trc])

########## Stage 2 - ATAC-58 to NINA-54 ############
c0.lab <- "ATAC-58"
cL.lab <- "NINA-54"
min.set.2 <- c("Ab","An","Rt","Mt","amp2","Bt")
mod2 <- rev.maj(c0.lab,cL.lab,min.set.2,norm=T)

cL2 <- fwd.mod(c0.lab,mod2$m,mod2$ff)$cL[trc]

# Adding zircon
zr.added <- 0.12/100
ee <- c(mod2$m,zr.added)
min.prop.2zrc <- ee/sum(ee)
names(min.prop.2zrc) <- c(min.set.2,"Zrn")
cL2zrc <- fwd.mod(c0.lab,min.prop.2zrc,mod2$ff)$cL[trc]
foo <- rbind(WR[c0.lab,trc],WR[cL.lab,trc],cL2[trc],cL2zrc[trc])

#### Major elements
fwd1 <- fwd.mod("ATAC-4",min.prop.1ampzrc,mod1amp$ff,norm=T)
fwd2 <- fwd.mod("ATAC-58",min.prop.2zrc,mod2$ff,norm=T)

#### Trace elements
ff1 <- seq(1,mod1amp$ff,by=-0.01)
fw.tr1 <- t(sapply(ff1,FUN=function(z){
    fwd.mod("ATAC-4",min.prop.1ampzrc,z,norm=T)$cL
}))

ff2 <- seq(1,mod2$ff,by=-0.01)
fw.tr2 <- t(sapply(ff2,FUN=function(z){
    fwd.mod("ATAC-58",min.prop.2zrc,z,norm=T)$cL
}))

y.axs2 <- c("Rb","Ba","La","Yb","Th","Zr","Ni","V")

diag.tr <- function(el){
  binary("SiO2",el,new=F)
  points(WR["ATAC-4","SiO2"],WR["ATAC-4",el],pch=15,cex=2,col="darkgreen",add=T)
  points(fwd1$cL["SiO2"],fwd1$cL[el],pch=16,cex=2,col="darkgreen",add=T)
  lines(fw.tr1[,"SiO2"],fw.tr1[,el],col="darkgreen",lwd=1.5)
  
  points(WR["ATAC-58","SiO2"],WR["ATAC-58",el],pch=15,cex=2,col="darkred",add=T)
  points(fwd2$cL["SiO2"],fwd2$cL[el],pch=16,cex=2,col="darkred",add=T)
  lines(fw.tr2[,"SiO2"],fw.tr2[,el],col="darkred",lwd=1.5)
}

# Figure 25.12
multiplePerPage(9,nrow=3,ncol=3,title=NULL)
for(j in 2:9){
    Plate(j)
    diag.tr(y.axs2[j-1])
}
