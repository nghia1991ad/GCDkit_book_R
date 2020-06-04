# The functions from Part_V_common_functions.r must be loaded beforehand:
# drag-and-drop the file onto the R-console or run source("Part_V_common_functions.r")

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

# Figure 25.9
windows(h=6,w=4)
spider(foo,"Boynton",ymin=1,ymax=100,col=c("red","darkblue","blue","royalblue"),pch=c(15,16,1,6),new=F)
windows(h=6,w=7)
spider(foo,"^Primitive Mantle..McDonough 1995",ymin=1,ymax=500,col=c("red","darkblue","blue","royalblue"),pch=c(15,16,1,6),new=F)
