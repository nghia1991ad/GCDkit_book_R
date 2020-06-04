loadData("boggy_plain.data")
windows(width=10,height=6)              # Open an empty window
par(mfrow=c(1,2))                       # Split it into two

# Plot 1
# Create an empty plot (type="n") with an appropriate range
plot(1,1,xlim=c(650,950),ylim=c(0,650),xlab=expression(T*degree*C),ylab="Zr (ppm)",type="n")
M <- seq(0.9,2.0,by=0.1)                # Setup the M values
col <- selectPalette(length(M),"blues") # The colours
for (i in 1:length(M)){
    curve(497644/exp(-3.8-0.85*(M[i]-1)+12900/(x+273.15)),lty="solid",lwd=2,col=col[i],add=TRUE)
}
# Legend colour-coded for individual M values
legend("bottomright",text.col=col,legend=M,title="M",bg="white",ncol=2)

# Plot 2
# Call plugin with arbitrary T, we do not use Zr sat values
sat.data <- zrSaturation(T=800)
# Create a plot with Boggy Plain data of appropriate range
plot(sat.data[,"M"],WR[,"Zr"],xlim=c(0.9,2.5),ylim=c(0,250),xlab="M",ylab="Zr (ppm)",pch=19)
tt <- seq(650,950,by=50)                # Setup the temperatures
col <- selectPalette(length(tt),"reds") # The colours
for (i in 1:length(tt)){
    curve(497644/exp(-3.8-0.85*(x-1)+12900/(tt[i]+273.15)),lty="solid",lwd=2,col=col[i],add=TRUE)

    # Prepare textual labels
    lab <- eval(substitute(expression(x*degree*C),list(x=tt[i])))
    M <- 0.9
    text(M,497644/exp(-3.8-0.85*(M-1)+12900/(tt[i]+273.15))-5,lab,adj=0,col=col[i])
}

addResults("sat.data")
binary("SiO2","TZr.sat.C",pch=15)
