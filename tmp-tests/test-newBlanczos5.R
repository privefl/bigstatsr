require(bigsnpr)
require(bigstatsr)

X <- attach.big.matrix("tmp2.desc")
print(dim(X))

binom2 <- snp_scaleBinom
k <- 10

# print(system.time(
#   true <- big_SVD(X, binom2, k = k)
# )) # 29 sec -> 100 sec with flashpca

print(system.time(
  test3 <- ParallelRandomSVD2.3(X, binom2, ncores = 2, K = k)
)) # 12 sec pour une itération, 14 sec with 2 cores

# true <- svd(X2[,], nu = k, nv = k)

print(system.time(
  test3 <- ParallelRandomSVD2.3(X, binom2, ncores = 2, K = k)
)) # 12 sec pour une itération, 14 sec with 2 cores

print(all.equal(test3$d, true$d[1:k]))
plot(test3$u, true$u)
s <- c(rep(FALSE, 19), TRUE)
plot(test3$v[s], true$v[s])
