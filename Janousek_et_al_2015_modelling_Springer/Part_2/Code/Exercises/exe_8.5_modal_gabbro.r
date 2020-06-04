mins <- read.table("gabbro_modal.data",sep="\t")
mins <- as.matrix(mins) # Needed prior to matrix multiplication
print(mins)

m <- c(0.5,0.3,0.2)
print(m%*%mins)

print(WRComp(mins,m))
