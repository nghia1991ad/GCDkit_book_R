x <- read.table("mantle_melting.data",sep="\t")
x <- as.matrix(x)
REE <- colnames(x)
prima <- x["PRIMA",]                # PRIMA composition
dm <- x["DM",]                      # DM composition
kd <- x[c(-1,-2),]                  # table of distribution coefficients
m <- read.table("mantle_melting_modal.data",sep="\t")
m <- as.matrix(m)                   # table of mineral props after melting
dd <- m%*%kd                        # bulk distrib. coeff. [Eq.(10.4)]
print(round(dd,3))

ff <- c(0.01,0.02,0.05,0.1,0.2)     # degrees of melting
# function calculating batch melt composition [Eq. (11.1)]
batch <- function(c0,ff,dd){
      out <- sapply(ff,function(i){
        z <- c0/(dd+i*(1-dd))
        return(z)
      })
      out<-t(out)
      rownames(out)<-ff
      return(out)
}

# Shallow melting, calculation 
shallow1 <- batch(prima,ff,dd["5",])# PRIMA
print(shallow1,3)
shallow2<-batch(dm,ff,dd["5",])     # DM
print(shallow2,3)

# Shallow melting, plotting - PRIMA is blue, DM green 
mantle1 <- rbind(prima,dm)          # Two mantle sources
col0<-c("darkblue","darkgreen")
spider(mantle1,"Boynton",0.1,100,pch=16,cex=1.5,lwd=1.5,col=col0,main="Shallow melting (5 kbar)")

col1 <- selectPalette(nrow(shallow1),"blues")
col2 <- selectPalette(nrow(shallow2),"greens")
shallow<-rbind(shallow1,shallow2)
col<-c(col1,col2)
spider(shallow,"Boynton",pch="",col=col,add=TRUE)# adds to existing
legend("bottomright",legend=rep(ff,2),pch=15,col=col,bg="white",ncol=2,title="PRIMA/DM")
  
 
# Deep melting, calculation
deep1<-batch(prima,ff,dd["15",])    # PRIMA
print(deep1,3)
deep2<-batch(dm,ff,dd["15",])       # DM
print(deep2,3)

# Deep melting, plotting - PRIMA is blue, DM green
mantle2 <- rbind(prima,dm)          # Two mantle sources
col <- c("darkblue","darkgreen")
spider(mantle2,"Boynton",0.1,100,pch=16,cex=1.5,lwd=1.5,col=col,main="Deep melting (15 kbar)")
col1 <- selectPalette(nrow(deep1),"blues")
col2 <- selectPalette(nrow(deep2),"greens")
deep <- rbind(deep1,deep2)
col <- c(col1,col2)
spider(deep,"Boynton",pch="",col=col,add=TRUE)
legend("bottomright",legend=rep(ff,2),pch=15,col=col,bg="white",ncol=2,title="PRIMA/DM")
