x <- read.table("sazava_fc2.data",sep="\t")
x <- data.matrix(x)
cc <- x[,-1]            # matrix, 1st column contains diff. magma,
                        # the remaining the mineral compositions
C0 <- x[,1]             # parental magma composition
ee <- lsfit(cc,C0,intercept=FALSE) 

# least-square solution
fc <- 1-ee$coeff[1]     # degree of fractional crystallization
f <- ee$coeff[-1]       # if these are normalized to sum up to 1
m <- f/sum(f)           # we get the mineral proportions
cat(round(100*fc,3),"% fc ","\n")

print(m*100,4)

cat("\nRsquared: ",sum(ee$residuals^2),"\n")
