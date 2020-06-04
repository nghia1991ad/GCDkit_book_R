x <- read.table("sazava_fc.data",sep="\t")
x <- as.matrix(x)       # dataframe needs to be transformed to
                        # a matrix for matrix multiplication
c0 <- x[,1]             # composition of parental magma
mins <- x[,-1]          # composition of fractionating phases
m <- c(0.5,0.3,0.2)     # mineral proportions in cumulate
fc <- 0.2               # degree of fractionation
cs <- mins%*%m          # cumulate composition [Eq.(6.14)]
cl <-(c0-cs*fc)/(1-fc)  # and of residual liquid [Eq. (8.2)]
x <- cbind(x,cs,cl)
colnames(x) <- c("tonalite",colnames(mins),"cumulate","dif.magma")
print(round(x,2))
