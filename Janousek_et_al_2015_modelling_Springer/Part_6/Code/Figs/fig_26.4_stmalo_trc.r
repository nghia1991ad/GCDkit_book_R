loadData("stmalo.data")
assignColVar("Melt_frac","reds")

# Plots - data exploration

# Figure 26.4
multiple("SiO2","Rb,Sr,Zr,Ni,Cr,V")
plate0YLim()     # to set zeroes as minima to all y axes
plateCex(2)
plateCexLab(2) 
