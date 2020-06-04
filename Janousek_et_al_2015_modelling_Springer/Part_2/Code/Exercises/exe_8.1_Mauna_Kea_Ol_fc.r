x <- read.table("MaunaKea.data",sep="\t")
c0 <- x[,1]                     # composition of parental magma
ol <- x[,2]                     # olivine composition
fc <- c(0.05,0.1,0.2,0.35)      # degrees of fractionation
for (ee in fc){ 
    y <- (c0-ol*i)/(1-ee)       # fractionated magma [Eq.(8.1)]
    x <- cbind(x,y)
}
colnames(x) <- c("C0","Ol",fc)
print(round(x,2))
