
pca <- prcomp(iris[-5], center = TRUE, scale. = TRUE)
test <- BKPC::kPCA(pca$x)
str(test)
plot(test$Es, log = "xy", pch = 20)
colMeans(pca$x)

# K <- tcrossprod(scale(iris[-5]))
# rowMeans(K)
# colMeans(K)
# eigs <- eigen(K)
# test$KPCs[1:5, 1:5]
# K[1:5, 1:5]

X <- pca$x
K <- fields::rdist(X)


dbl_center <- function(K) {
  means <- colMeans(K)
  sweep(sweep(K, 2, means), 1, means) + mean(means)
}


-log(test$K[1:5, 1:5]) / test$theta
K[1:5, 1:5]^2

K2 <- exp(- K^2 * test$theta)
K2[1:5, 1:5]
K3 <- dbl_center(K2)
eigs <- eigen(K3)
all.equal(eigs$values, test$Es)

tmp <- K3 %*% eigs$vectors %*% diag(sqrt(150 / eigs$values))
tmp[1:5, 1:5] / test$KPCs[1:5, 1:5]
plot(tmp, pch = 20, col = iris$Species)
plot(tmp[, 3:4], pch = 20, col = iris$Species)

## Compute K3
d <- apply(X, 1, crossprod)
K <- -sweep(sweep(2 * tcrossprod(X), 2, d), 1, d)
K[1:5, 1:5]
fields::rdist(X[1:5, ])^2
K2 <- exp(- K * test$theta)
K3 <- dbl_center(K2)
eigs <- eigen(K3)
all.equal(eigs$values, test$Es)

## compute K3 %*% x
