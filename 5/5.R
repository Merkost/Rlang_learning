A <- matrix(seq(1, 9),3 ,3)

B <- matrix(seq(2, 10),3 ,3)

C <- A + B

C <- C * c(0,1)

newA <- matrix(seq(1, 16),4 ,4)
D <- t(newA) %*% newA
det(D)
solve(D, tol = 3.55e-18)

# create matrix A and B using given equations
A <- rbind(c(1, 3),
           c(2, 4))
b <- c(5, 8)

# Solve them using solve function in R
solve(A, b)

arr <- array(1:120, dim = c(2,3,4,5))