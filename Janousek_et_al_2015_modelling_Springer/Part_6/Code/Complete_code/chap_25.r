####################################################
#
# Part VI, Chapter 25: Atacazo-Ninahuilca Worked example
#
####################################################

# The functions from Part_VI_common_functions.r must be loaded beforehand:
#source("Part_VI_common_functions.r") #or drag-and-drop the file onto the R-console

########## Definition, data loading ############
loadData("atacazo.data")
min.tab <- read.table("atacazo_mins.data",sep="\t")
kd.tab <- read.table("atacazo_kd.data",sep="\t")
mjrs <- c("SiO2","Al2O3","Fe2O3","MgO","CaO","Na2O","K2O","TiO2")
trc <- c("La","Ce","Nd","Sm","Eu","Gd","Dy","Er","Yb","Rb","Ba","Th","Nb","Sr","Zr","Y","V","Cr","Ni")

########## Stage 1 - ATAC-4 to ATAC-58 ############
c0.lab <- "ATAC-4"
cL.lab <- "ATAC-58"

#### Model with Cpx
min.set.1cpx <- c("Cpx","Opx","Ab","An","Rt","Mt")
mod1cpx <- rev.maj(c0.lab,cL.lab,min.set.1cpx,norm=T)
print(mod1cpx,3)

print(plagio(mod1cpx$m["An"],mod1cpx$m["Ab"]),2)

cL1cpx <- fwd.mod(c0.lab,mod1cpx$m,mod1cpx$ff)$cL[trc]
print(sum(((WR[cL.lab,trc]-cL1cpx[trc])/WR[cL.lab,trc])^2)) # Sum(R2) for traces

foo <- rbind(WR[c0.lab,trc],WR[cL.lab,trc],cL1cpx[trc])
# Figure 25.7
windows(h=6,w=4)
spider(foo,"Boynton",ymin=1,ymax=100,col=c("red","darkblue","blue"),pch=c(15,16,1),new=F)
windows(h=6,w=7)
spider(foo,"^Primitive Mantle..McDonough 1995",ymin=1,ymax=500,col=c("red","darkblue","blue"),pch=c(15,16,1),new=F) 
       
#### Model with amp
min.set.1amp <- c("Opx","Ab","An","Rt","Mt","Amp")
mod1amp <- rev.maj(c0.lab, cL.lab,min.set.1amp,norm=T)
print(mod1amp,3)

cL1amp <- fwd.mod(c0.lab,mod1amp$m,mod1amp$ff)$cL[trc]

# Adding zircon
zr.added <- 0.008/100 
ee <- c(mod1amp$m,zr.added)
min.prop.1ampzrc <- ee/sum(ee)
names(min.prop.1ampzrc)<- c(min.set.1amp,"Zrn")
cL1ampzrc <- fwd.mod(c0.lab,min.prop.1ampzrc,mod1amp$ff)$cL[trc]
foo <- rbind(WR[c0.lab,trc],WR[cL.lab,trc],cL1amp[trc],cL1ampzrc[trc])

# Figure 25.9
windows(h=6,w=4)
spider(foo,"Boynton",ymin=1,ymax=100,col=c("red","darkblue","blue","royalblue"),pch=c(15,16,1,6),new=F)
windows(h=6,w=7)
spider(foo,"^Primitive Mantle..McDonough 1995",ymin=1,ymax=500,col=c("red","darkblue","blue","royalblue"),pch=c(15,16,1,6),new=F)

#### Model with cpx and amp (does not work !)
min.set.1both <- c("Opx","Cpx","Ab","An","Rt","Mt","Amp")
mod1both <- rev.maj(c0.lab,cL.lab,min.set.1both)
print(mod1both,3)

#### Reverse trace elements
min.set.1ampzrc.trc <- c("Opx","Ab","Mt","Amp","Zrn")
rev.tr.fc(c0.lab,cL.lab,min.set.1ampzrc.trc)

########## Stage 2 - ATAC-58 to NINA-54 ############
c0.lab <- "ATAC-58"
cL.lab <- "NINA-54"
min.set.2 <- c("Ab","An","Rt","Mt","amp2","Bt")
mod2 <- rev.maj(c0.lab,cL.lab,min.set.2,norm=T)
print(mod2,3)

cL2 <- fwd.mod(c0.lab,mod2$m,mod2$ff)$cL[trc]

# Adding zircon
zr.added <- 0.12/100
ee <- c(mod2$m,zr.added)
min.prop.2zrc <- ee/sum(ee)
names(min.prop.2zrc) <- c(min.set.2,"Zrn")
cL2zrc <- fwd.mod(c0.lab,min.prop.2zrc,mod2$ff)$cL[trc]
foo <- rbind(WR[c0.lab,trc],WR[cL.lab,trc],cL2[trc],cL2zrc[trc])

# Figure 25.9
windows(h=6,w=4)
spider(foo,"Boynton",ymin=1,ymax=100,col=c("darkblue","darkgreen","green","chartreuse2"),pch=c(15,16,1,6),new=F)
windows(h=6,w=7)
spider(foo,"^Primitive Mantle..McDonough 1995",ymin=1,ymax=500,col=c("darkblue","darkgreen","green","chartreuse2"),pch=c(15,16,1,6),new=F)

########## Final graphs ############

#### Major elements
fwd1 <- fwd.mod("ATAC-4",min.prop.1ampzrc,mod1amp$ff,norm=T)
fwd2 <- fwd.mod("ATAC-58",min.prop.2zrc,mod2$ff,norm=T)
y.axs <- c("Al2O3","Fe2O3","MgO","CaO","Na2O","K2O")

diag.maj <- function(ox){
  xmin=0.95*min(WR[,"SiO2"],fwd1$cS[1,"SiO2"],fwd2$cS[1,"SiO2"])
  xmax=1.05*max(WR[,"SiO2"],fwd1$cS[1,"SiO2"],fwd2$cS[1,"SiO2"])
  ymin=0.95*min(WR[,ox],fwd1$cS[1,ox],fwd2$cS[1,ox])
  ymax=1.05*max(WR[,ox],fwd1$cS[1,ox],fwd2$cS[1,ox])
  
  binary("SiO2",ox,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,new=F)
  points(fwd1$cS[1,"SiO2"],fwd1$cS[1,ox],pch=17,cex=2,col="darkgreen",add=T)
  points(WR["ATAC-4","SiO2"],WR["ATAC-4",ox],pch=15,cex=2,col="darkgreen",add=T)
  points(fwd1$cL["SiO2"],fwd1$cL[ox],pch=16,cex=2,col="darkgreen",add=T)
  lines(c(fwd1$cS[1,"SiO2"],WR["ATAC-4","SiO2"]),c(fwd1$cS[1,ox],WR["ATAC-4",ox]),col="darkgreen",lwd=1,lty="dashed")
  lines(c(WR["ATAC-4","SiO2"],fwd1$cL["SiO2"]),c(WR["ATAC-4",ox],fwd1$cL[ox]),col="darkgreen",lwd=1.5)
  
  points(fwd2$cS[1,"SiO2"],fwd2$cS[1,ox],pch=17,cex=2,col="darkred",add=T)
  points(WR["ATAC-58","SiO2"],WR["ATAC-58",ox],pch=15,cex=2,col="darkred",add=T)
  points(fwd2$cL["SiO2"],fwd2$cL[ox],pch=16,cex=2,col="darkred",add=T)
  lines(c(fwd2$cS[1,"SiO2"],WR["ATAC-58","SiO2"]),c(fwd2$cS[1,ox],WR["ATAC-58",ox]),col="darkred",lwd=1,lty="dashed")
  lines(c(WR["ATAC-58","SiO2"],fwd2$cL["SiO2"]),c(WR["ATAC-58",ox],fwd2$cL[ox]),col="darkred",lwd=1.5)
}


# Figure 25.11
multiplePerPage(6,nrow=2,ncol=3,title=NULL)
for(j in 1:6){
    Plate(j)
    diag.maj(y.axs[j])
}
plateCex(1.5)
plateCexLab(2) 

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
plateCex(1.5)
plateCexLab(2)
