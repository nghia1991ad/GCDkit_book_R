loadData("atacazo.data")
addResultsIso()

# Figure 25.6
multiplePerPage(3,nrow=1,ncol=3,title=NULL)
Plate(1)
binary("Rb","Ni",log="xy",xmin=3,ymin=3,xmax=70,ymax=70,new=F)
Plate(2)
binary("Ba","Cr",log="xy",xmin=5,ymin=5,xmax=1100,ymax=1100,new=F)
Plate(3)
binary("La","Yb",log="xy",xmin=0.3,ymin=0.3,xmax=15,ymax=15,new=F)
plateCexLab(1.6)
plateCex(2) 
