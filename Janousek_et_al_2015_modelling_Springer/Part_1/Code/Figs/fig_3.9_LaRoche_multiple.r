loadData("sazava.data")
groupsByLabel("Intrusion")
plotDiagram("LarochePlut",FALSE)
figRemove()             # Less cluttered, no field labels
figMulti(nrow=1,ncol=3) # Three plots in a single row

plateCex(2)             # symbols size, the whole plate
plateCexMain(2)         # main title
plateCexLab(1.8)        # axis labels size
plateBW()               # set plate to B&W
