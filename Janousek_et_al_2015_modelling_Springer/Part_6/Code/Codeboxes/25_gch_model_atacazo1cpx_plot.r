########## Stage 1 - ATAC-4 to ATAC-58 1 cpx graphics ############

foo <- rbind(WR[c0.lab,trc],WR[cL.lab,trc],cL1cpx[trc])
# Figure 25.7
windows(h=6,w=4)
spider(foo,"Boynton",ymin=1,ymax=100,col=c("red","darkblue","blue"),pch=c(15,16,1),new=F)
windows(h=6,w=7)
spider(foo,"^Primitive Mantle..McDonough 1995",ymin=1,ymax=500,col=c("red","darkblue","blue"),pch=c(15,16,1),new=F) 
