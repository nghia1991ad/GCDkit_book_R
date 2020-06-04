loadData("ANDEAN_ARC.csv")

# Panel a
plotDiagram("PeceTaylor",F)
figCol("aquamarine3")
addContours()

# Panel b
figRedraw()
filledContourFig()
