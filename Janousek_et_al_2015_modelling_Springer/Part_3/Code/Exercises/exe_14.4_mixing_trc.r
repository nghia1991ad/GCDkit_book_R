elems <- c("Rb","Zr","Nb")
ucc <- c(112,190,25); names(ucc) <- elems
morb <- c(0.56,74,2.33); names(morb) <- elems
plot(1,1,xlim=c(0,120),ylim=c(0,35),xlab="Rb (ppm)",ylab="Zr/Nb",type="n")     # Only axes are set up
 
# Parameters of the mixing hyperbola (Table 11.1)
AA <- ucc["Zr"]- morb["Zr"]
BB <- morb["Nb"]-ucc["Nb"]
CC <- ucc["Nb"]*morb["Rb"]-morb["Nb"]*ucc["Rb"]
DD <- ucc["Rb"]*morb["Zr"]-morb["Rb"]*ucc["Zr"]
curve((-AA*x-DD)/(BB*x+CC),from=morb["Rb"],to=ucc["Rb"],add=TRUE,col="darkred") # Eq. 11.29

points(morb["Rb"],morb["Zr"]/morb["Nb"],pch=19)
text(morb["Rb"],morb["Zr"]/morb["Nb"],pos=3,"MORB")
points(ucc["Rb"],ucc["Zr"]/ucc["Nb"],pch=19)
text(ucc["Rb"],ucc["Zr"]/ucc["Nb"],pos=3,"UCC")
x <- seq(morb["Rb"],ucc["Rb"],length=11)
y <- (-AA*x-DD)/(BB*x+CC)   # Eq. 11.30
points(x,y,pch=1)
