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

#### Model with Cpx
min.set.1cpx <- c("Cpx","Opx","Ab","An","Rt","Mt")
mod1cpx <- rev.maj(c0.lab,cL.lab,min.set.1cpx,norm=T)

cL1cpx <- fwd.mod(c0.lab,mod1cpx$m,mod1cpx$ff)$cL[trc]

foo <- rbind(WR[c0.lab,trc],WR[cL.lab,trc],cL1cpx[trc])
# Figure 25.7
windows(h=6,w=4)
spider(foo,"Boynton",ymin=1,ymax=100,col=c("red","darkblue","blue"),pch=c(15,16,1),new=F)
windows(h=6,w=7)
spider(foo,"^Primitive Mantle..McDonough 1995",ymin=1,ymax=500,col=c("red","darkblue","blue"),pch=c(15,16,1),new=F) 
