x <- read.table("ttg2.data",sep="\t")
x <- data.matrix(x)
dmat <- x[,-(1:2)]-1    # bulk distrib. coeffs - 1 [Eq. (12.3)]
cv <- log(x[,2]/x[,1])  # log ratios of differentiated/primitive magma comp. [Eq. (12.4)]
ee <- lsfit(dmat,cv,intercept=FALSE)
mm <- ee$coeff
ff <- exp(sum(mm))      # fraction of melt remaining [Eq.(13.7)]
cat(round((1-ff)*100,1),"% fractional crystallization ","\n")
m <- mm/log(ff)*100     # mineral proportions [Eq. (13.8)]
print(round(m,1))
