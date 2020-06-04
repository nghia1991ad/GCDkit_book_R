x <- read.table("mantle_conc.data",sep="\t")# Read rock comp.
kd <- read.table("mantle_kd.data",sep="\t")# Partitioning coeff
c0 <- x[,1]         # Lherzolite
names(c0) <- rownames(x)
cl <- x[,2]         # Basalt
names(cl) <- rownames(x)
ratio <- cl/c0
print(sort(ratio),3)

ff <- 1/ratio["La"]
cat("The degree of melting is", round(ff*100,1),"%.","\n")

Rb<-cl["Rb"]*ff
c0["Rb"]<-Rb
cat("Rb in the lherzolite is", round(Rb,2),"ppm.","\n")

cv <- c0/cl-ff
m <- lsfit(kd,cv,intercept=0)$coeff
m <- m/(1-ff)       # Eq. (12.15)
print(m,2)
