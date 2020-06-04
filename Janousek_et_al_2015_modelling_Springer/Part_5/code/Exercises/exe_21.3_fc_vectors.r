WR <- read.table("blatna.data",sep="\t")    # Read whole-rock data
kd <- read.table("hanson.data",sep="\t")    # Partition coefficients

plot(WR[,"Ba"],WR[,"Sr"],xlab="Ba",ylab="Sr",log="xy",pch=15,ylim=c(250,550)) # Binary plot (plain R)

Ba0 <- 1500
Sr0 <- 500
Ba <- Ba0*0.9^(kd[,"Ba"]-1)
Sr <- Sr0*0.9^(kd[,"Sr"]-1)
cols <- c("darkgreen","sienna","red","blue")
arrows(Ba0, Sr0,Ba,Sr,col=cols,lwd=1.5)
legend("bottomright",text.col=cols,legend=rownames(kd)) 
