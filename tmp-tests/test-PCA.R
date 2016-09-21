pacman::p_load(mvtnorm)

N <- 2000
M <- 500

s <- matrix(rnorm(N * M), N, M)

sigma <- crossprod(s)

X <- mvtnorm::rmvnorm(N, sigma = sigma)
X2 <- sweep(X, 2, colMeans(X), '-')
X3 <- MASS::mvrnorm(N, mu = rep(0, M), Sigma = sigma)
X2 <- sweep(X3, 2, colMeans(X3), '-')

pca <- prcomp(X3)
plot(pca$x)

K <- crossprod(X2)

eig <- eigen(K, symmetric = TRUE)
plot(eig$values)

pca2 <- princomp(X)

plot(pca$sdev, pca2$sdev)
abline(0, 1, col = "red")

plot(pca$x[, 1], pca2$scores[, 1])
abline(0, -1, col = "red")

scores2 <- X2 %*% sweep(eig$vectors, 2, sqrt(eig$values), '/')
scores3 <- X2 %*% eig$vectors

plot(scores3[, 1], pca$x[, 1])
abline(0, -1, col = "red")
diff1 <- abs(pca$x - scores3)
diff2 <- abs(pca$x + scores3)
diff <- pmin(diff1, diff2)
print(max(diff))

diff1 <- abs(pca2$scores - scores3)
diff2 <- abs(pca2$scores + scores3)
diff <- pmin(diff1, diff2)
print(max(diff))
