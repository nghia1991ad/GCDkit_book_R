# The functions from Part_V_common_functions.r must be loaded beforehand:
# drag-and-drop the file onto the R-console or run source("Part_V_common_functions.r")

########## Definition, data loading ############
loadData("atacazo.data")
min.tab <- read.table("atacazo_mins.data",sep="\t")
kd.tab <- read.table("atacazo_kd.data",sep="\t")
mjrs <- c("SiO2","Al2O3","Fe2O3","MgO","CaO","Na2O","K2O","TiO2")
trc <- c("La","Ce","Nd","Sm","Eu","Gd","Dy","Er","Yb","Rb","Ba","Th","Nb","Sr","Zr","Y","V","Cr","Ni")

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

# Figure 25.10
windows(h=6,w=4)
spider(foo,"Boynton",ymin=1,ymax=100,col=c("darkblue","darkgreen","green","chartreuse2"),pch=c(15,16,1,6),new=F)
windows(h=6,w=7)
spider(foo,"^Primitive Mantle..McDonough 1995",ymin=1,ymax=500,col=c("darkblue","darkgreen","green","chartreuse2"),pch=c(15,16,1,6),new=F)
