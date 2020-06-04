x <- read.table("paragneiss_melting.data",sep="\t")
x <- as.matrix(x)       # convert for matrix multiplication
c0 <- x[,1]             # paragneiss composition
cl <- x[,2]             # melt chemistry
mins <- x[,-c(1,2)]     # mineral compositions
ff <- 0.40              # degree of partial melting
res <- (c0-ff*cl)/(1-ff)# mass balance for residue [Eq. (8.3)]
print(round(res,2))
ee <- lsfit(mins,res,intercept=FALSE) # least-square fit gives
m <- ee$coeff           # mineral proportions in the residue
print(round(m,2))
