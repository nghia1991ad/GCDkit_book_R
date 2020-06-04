loadData("stmalo.data")
assignColVar("Melt_frac","reds")

# Plots - data exploration
# Figure 26.3

multiple("SiO2","Al2O3,Fe2O3,MgO,CaO,Na2O,K2O")
plateCex(2)
plateCexLab(2) 


# Figure 26.4
multiple("SiO2","Rb,Sr,Zr,Ni,Cr,V")
plate0YLim()     # to set zeroes as minima to all y axes
plateCex(2)
plateCexLab(2) 
