loadData("dolerites.data")

# Panel a
ee<-spider.contour("Boynton","MgO",colour.palette="terrain.colors",0.1,100,cex=1.5)

# Panel b
ee<-spider2norm(WR,"Boynton","Lu",0.1,10,pch=1,col="darkgreen",cex=1.5)
ee<-spider2norm(WR,"Boynton","Lu",field=TRUE,fill.col=TRUE,shaded.col="khaki",add=TRUE)
