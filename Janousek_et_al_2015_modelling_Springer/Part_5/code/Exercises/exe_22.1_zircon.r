loadData("noisy.data",sep="\t")                 # Read whole-rock data
mjrs <- c("SiO2","Al2O3","FeOt","MgO","MnO","CaO","Na2O","K2O","TiO2")
trc <- c("Zr","Rb","Y")
c0.1 <- WR["a13",c(mjrs,trc)]
mincomp <- read.table("mincomp.data",sep="\t")  # Mineral data
mincomp <- as.matrix(mincomp)
minprop1 <- c(0.45,0.5,0.05,0)
names(minprop1) <- c("Pl","Amp","Mt","Zrn")
cs.1 <- as.vector(minprop1%*% mincomp)
names(cs.1) <- colnames(mincomp)
kd <- read.table("kd.data",sep="\t")            # Kd data
kd <- as.matrix(kd)

t.c <- function(si){
    z <- 1020-(si-56)/16*300
    return(z)
}


cl.f <- function(ff,c0,cs,minprop){
    # Major elements
    maj <- (c0[mjrs]-(1-ff)*cs[mjrs])/ff 
    # Trace elements (omitting solubility)
    D <- minprop%*%kd # D values, not accounting for Zrn sat!
    tr <- c0[trc]*ff^(D[1,trc]-1)
    
    # Saturation values
    milcats <- millications(maj[mjrs])
    sat.data <- zrSaturation(milcats,T=t.c(maj["SiO2"]),Zr=WR["a13","Zr"]) # call Saturation plugin
    Zr.sat <- sat.data["Zr.sat"]
    M <- sat.data["M"]

    # Correction for Zrn
    if(tr["Zr"]<=Zr.sat){
        tr.c <- tr
        zrc.prop <- 0
    }else{
        zr.excess <- tr["Zr"]-Zr.sat
        zrc.prop <- zr.excess/497644
        m.pr <- minprop
        m.pr["Zrn"] <- zrc.prop
        m.pr <- m.pr/sum(m.pr)
        D.c <- m.pr%*%kd
        tr.c <- c0[trc]*ff^(D.c[1,trc]-1)
        tr.c["Zr"] <- Zr.sat
    }
    z <- c(maj,M,Zr.sat,zrc.prop,tr,tr.c)
    names(z) <- c(mjrs,"M","Zr.sat","Zrn.prop",paste(trc,"nosat",sep="_"),trc)
    return(z)
}


f.1 <- seq(1,0.42,by=-0.01)
qq <- sapply(f.1,cl.f,c0=c0.1,cs=cs.1,minprop=minprop1)
fwd.mod <- t(qq)

# Binary plot
binary("SiO2","Zr",pch=15,ymax=300) 
lines(fwd.mod[,"SiO2"],fwd.mod[,"Zr_nosat"],lty="dashed",col=2)
lines(fwd.mod[,"SiO2"],fwd.mod[,"Zr"],col=2) 

# Apparent Kd Zr for zircon
print(497644/fwd.mod[,"Zr"])

# Find saturation point
j <- which.max(fwd.mod[,"Zr"])
c0.2 <- fwd.mod[j,]
points(c0.2["SiO2"],c0.2["Zr"],cex=2,pch=8,col="purple")

zrc.amount <- 0.0025
f.2 <- f.1[j:length(f.1)]/f.1[j]
minprop2 <- minprop1
minprop2["Zrn"] <- zrc.amount
minprop2 <- minprop2/sum(minprop2)
qq <- sapply(f.2,cl.f,c0=c0.2,cs=cs.1,minprop=minprop2)
fwd.mod2 <- t(qq)
lines(fwd.mod2[,"SiO2"],fwd.mod2[,"Zr_nosat"],col="purple")
