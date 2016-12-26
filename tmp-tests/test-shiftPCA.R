# shifted towards 0 even with no scaling

require(bigsnpr)
require(bigstatsr)

popres <- AttachBigSNP("../thesis-celiac/popres/backingfiles/popres_sub1.bk")
X <- popres$genotypes
print(dim(X))
n <- nrow(X)

noscale <- big_scale(F, F)
k <- 10


ind <- sample(n, 1000)

print(system.time(
  true <- big_SVD(X, noscale, k = k, ind.train = ind)
)) # 29 sec -> 100 sec with flashpca

col <- 1:n %in% ind + 1
pred <-

scores <- matrix(NA, n, k)
scores[ind, ] <- true$u %*% diag(true$d)
scores[-ind, ] <- X[-ind, ] %*% true$v
scores <- scores[, -1]
plot(scores, col = col, pch = 19, cex = 0.5)
plot(scores[, 3:4], col = col, pch = 19, cex = 0.5)
