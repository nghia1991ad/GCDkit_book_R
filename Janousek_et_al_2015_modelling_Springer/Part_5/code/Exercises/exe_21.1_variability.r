# Variability
chpuy <- read.table("chpuy.data",sep="\t")
s <- apply(chpuy,2,sd,na.rm=T)  # na.rm=T is needed to calculate even for elems with missing values
m <- apply(chpuy,2,mean,na.rm=T)
variability <- s/m              # Eq. (21.1)
print(rev(sort(variability)),2)

# Comparison with NMORB
hofmann <- read.table("hofmann.data",sep="\t")
hofmann <- as.matrix(hofmann)

# Select elements for which we do have the MORB variability data
v.el <- colnames(hofmann)[!is.na(hofmann["V.MORB",])]
x.data <- hofmann["V.MORB",v.el]
plot(x.data,variability[v.el],xlim=c(0,0.5),ylim=c(0,0.5),pch=15,xlab="Variability, MORB",ylab="Variability, Ch. Des Puys")
text(x.data,variability[v.el],v.el,pos=4,offset=0.5)
abline(0,1)

# Order the elements in decreasing incompatibility
v.ordered <- rev(sort(variability [colnames(hofmann)]))

# Select the data and normalization values
data <- chpuy[,names(v.ordered)]
norm <- hofmann["PM",names(v.ordered)]

# Plotting [GCDkit]
norm <- t(as.matrix(norm))
rownames(norm) <- "Prim. mantle (Hofmann 1988) [CP order]"
spider(data,norm,pch=15,ymin=1,ymax=200)
print(results)
