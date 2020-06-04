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
