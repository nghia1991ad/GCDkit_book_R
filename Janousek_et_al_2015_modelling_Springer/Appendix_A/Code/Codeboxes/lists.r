x1 <- c("Luckovice","9 km E of Blatna","disused quarry")
x2 <- "melamonzonite"
x3 <- c(47.31,1.05,14.94,7.01,8.46,10.33)
names(x3) <- c("SiO2","TiO2","Al2O3","FeO","MgO","CaO")
luckovice <- list(ID="Gbl-4",Locality=x1,Rock=x2,major=x3)
luckovice

luckovice[[1]]

luckovice$Rock 
luckovice[[3]]

luckovice[[2]][3]

luckovice$major[c("SiO2","Al2O3")]
