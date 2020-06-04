loadData("sazava.data")
spider(WR,"Boynton",0.1,1000,pch=labels$Symbol,col=labels$Colour)
spider(WR,"Boynton",field=TRUE,density=0.02,angle=45, col="gray",fill.col=FALSE,add=TRUE) 
