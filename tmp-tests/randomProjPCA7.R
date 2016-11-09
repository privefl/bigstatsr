require(bigstatsr)

M <- 781
N <- 1510
s <- matrix(rnorm(N * M, sd = 1), N, M)
sigma <- crossprod(s) / N
x <- MASS::mvrnorm(N, mu = rep(0, M), Sigma = sigma) #matrix(rnorm(N*M), N)
X <- as.big.matrix(x, type = "char")

I = 10
test <- big_randomSVD2(X, fun.scaling = big_center, K = 5,
                       I = I, block.size = 1e3, TOL = 1e-8)
scores <- test$u %*% diag(test$d)

# test3 <- big_randomSVD2(X, fun.scaling = big_center, K = 2,
#                         I = I, L = 5,
#                       block.size = 1e3)
#
#
# str(test)

test2 <- prcomp(X[,], center = TRUE, scale. = FALSE)
#str(test2)

# function for comparing
diffPCs <- function(test, rot) {
  k <- ncol(test)
  diff1 <- 2 * abs(test - rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff2 <- 2 * abs(test + rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff <- pmin(diff1, diff2)
  mean(diff)
}

#print(diffPCs(test$H1.u$u, test$H2.u$u))

# print(diffPCs(test$u, test3$u))
# print(diffPCs(test$v, test3$v))
print(diffPCs(test$u %*% diag(test$d), test2$x))
# print(diffPCs(test3$u %*% diag(test3$d), test2$x))
print(diffPCs(test$v, test2$rotation))
# print(diffPCs(test3$v, test2$rotation))
