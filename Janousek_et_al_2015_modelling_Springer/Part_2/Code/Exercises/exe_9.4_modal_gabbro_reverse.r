x <- read.table("gabbro_modal2.data",sep="\t")
x <- as.matrix(x)       # convert data frame to numeric matrix
rock <- x[,1]           # whole-rock composition of the gabbro
mins <- x[,-1]          # mineral compositions
m <- lsfit(mins,rock,intercept=FALSE)$coeff
print(round(m*100),2)

Mode(rock,t(mins))      # transpose, GCDkit expects oxides in columns
ModeC(rock,t(mins))     # the same, but constrained least-squares
