x <- read.table("paragneiss_melting.data",sep="\t")
x <- as.matrix(x)       # convert for matrix multiplication
c0 <- x[,1]             # paragneiss composition
cc <- x[,-1]            # melt and residual minerals matrix
ee <- lsfit(cc,c0,intercept=FALSE)
ff <- ee$coeff[1]       # degree of partial melting
z <- ee$coeff[-1]       # if these are normalized to sum up to 1
m <- z/sum(z)           # we get the mineral proportions
cat(round(100*ff,3),"% partial melting of:","\n")
print(m*100,4)
