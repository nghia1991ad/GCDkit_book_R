sazava <- read.table("sazava.data",sep="\t")
oxides <- c("MgO","CaO","Na2O","K2O")
boxplot(sazava[,oxides],col=c("khaki","gray","red","blue"))
summary(sazava[,oxides])
