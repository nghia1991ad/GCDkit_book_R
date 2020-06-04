A <- matrix(c(1,3,0.9,2,0.2,7,-0.7,-1,-2),3,3)
A
y <- c(21,24,27)
x <- solve(A,y)
x
# checking the result
A%*%x
