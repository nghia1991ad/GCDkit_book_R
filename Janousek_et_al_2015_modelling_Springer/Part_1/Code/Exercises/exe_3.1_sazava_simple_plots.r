sazava <- read.table("sazava.data",sep="\t")
windows(width=10,height=5)  # Empty window of correct size
par(mfrow=c(1,2))           # Split screen for 2 graphs
par(mar=c(4,4,1,1))         # Outer margins for each of the graphs

plot(sazava[,"SiO2"],sazava[,"CaO"],cex=1.2,xlab=expression(SiO[2]),ylab="CaO",pch=sazava[,"Symbol"], xlim=c(49,75),ylim=c(0,15))
text(sazava[,"SiO2"],sazava[,"CaO"],rownames(sazava),pos=4,cex=0.7)

abline(0,0.1)

plot(sazava[,"Zr"],sazava[,"Ba"],xlab="Zr",ylab="Ba",pch=15,cex=1.5,log="xy")
lq <- lm(log10(sazava[,"Ba"])~log10(sazava[,"Zr"]))

# lq is an arbitary variable name
# note the logarithms of the base 10 in the formula
lq 
abline(lq,lty=2,lwd=2,col="darkgreen")


# GCDkit solution
loadData("sazava.data")
binary("SiO2","CaO",IDlabels=1,cex=1.2)
abline(0,0.1)

binary("Zr","Ba",log="xy",pch=15,cex=1.5,col="black",fit=TRUE)

figMain("Demonstration")    # Add main title
figXlab("Zirconium (ppm)")  # Modify x axis label
figCol("darkred")           # Change colour of the symbols
figCex(2)                   # Size of the symbols
figCexLab(0.8)              # Scaling of the axes labels
figCexMain(1.5)             # Size of the main title
figXlim(c(10,300))          # Limits of the x axis
figUser("pch=\"+\"")        # Plotting symbol
figUser("main=\"My plot\";las=2;font.main=4;bg=\"khaki\";cex.main=2;col.main=\"blue\"")
figBw()                     # Set everything to black and white
