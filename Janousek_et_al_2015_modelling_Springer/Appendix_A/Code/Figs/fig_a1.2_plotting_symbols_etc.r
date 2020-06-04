windows(width=10, height=6, pointsize = 10)     # Open an empty window of correct size
par(mfrow=c(1,2))                               # Split the plotting window into two
   
 # Plotting symbols  
    par(mai = c(0.5, 0.5, 0.5, 0.5))
    ipch <- 1:20
    iy <- 3 + 4 - (ipch - 1)%%5
    ix <- (ipch - 1)%/%5
    plot(ix, iy, xlim = c(-0.5, 3.5), type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1)
    for (i in ipch) {
        points(ix[i], iy[i], pch = i, col = 1, bg = "yellow", 
            cex = 3)
        text(ix[i] - 0.4, iy[i], i, col = "black",cex=1.3)
    }
    box()
    
# Plotting colours
    
    col.list <-c("standard","cm.colors", "heat.colors", "terrain.colors","topo.colors", "rainbow","grays","reds", "blues", "greens", "cyans","violets", "yellows","jet.colors")
        
    n.show <- 10
    par(mai = c(0,0, 0, 0))
    ipch <- 0:(n.show - 1)
    by <- n.show
    ix <- 1:n.show
    plot(1, 1, type = "n", xlim = c(-0.5, max(ix) + n.show/10), ylim = c(length(col.list) + 0.5, -0.5), axes = FALSE, xlab = "", ylab = "",asp=1)
    ee <- sapply(1:length(col.list), function(j) {
                col <- .definedPalettes(col.list[j], n.show)
                if(col.list[j]=="standard") col<-c(col[1:8],rep("#FFFFFF",n.show-8))
                iy <- rep(j,n.show)
                points(ix, iy, pch = 15, col = col, cex = 4)
                mtext(col.list[j],line=-4,at=j,col="black",side=2,las=1,adj=1,cex=1.3)
    })
mtext(as.character(0:(n.show-1)),at=1:n.show,side=3,line=-3,cex=1.3)
      
