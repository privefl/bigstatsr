Rcpp::sourceCpp('tmp-save/prodMatVec2.cpp')
Rcpp::sourceCpp('src/prodMatVec.cpp')
require(bigmemory)

N <- 1e4
M <- 1e4
X <- big.matrix(N, M, shared = FALSE)
X[] <- rnorm(length(X))

y <- rnorm(M)
y2 <- rnorm(N)

p.true <- as.numeric(X[,] %*% y)
cp.true <- as.numeric(crossprod(X[,], y2))

require(microbenchmark)
print(microbenchmark(
  test <- pMatVec4(X@address, y, 1:N, 1:M),
  # test2 <- sub_pMatVec4(X@address, y, 1:N, 1:M),
  test3 <- cpMatVec4(X@address, y2, 1:N, 1:M),
  # test4 <- sub_cpMatVec4(X@address, y2, 1:N, 1:M),
  times = 50
))
print(all.equal(test, p.true))
# print(all.equal(test2, p.true))
print(all.equal(test3, cp.true))
# print(all.equal(test4, cp.true))


ind.row <- sort(sample(N, N/2))
ind.col <- sort(sample(M, M/2))

p.true <- as.numeric(X[ind.row, ind.col] %*% y[ind.col])
cp.true <- as.numeric(crossprod(X[ind.row, ind.col], y2[ind.row]))

print(microbenchmark(
  test <- pMatVec4(X@address, y[ind.col], ind.row, ind.col),
  # test2 <- sub_pMatVec4(X@address, y[ind.col], ind.row, ind.col),
  test3 <- cpMatVec4(X@address, y2[ind.row], ind.row, ind.col),
  # test4 <- sub_cpMatVec4(X@address, y2[ind.row], ind.row, ind.col),
  times = 50
))
print(all.equal(test, p.true))
# print(all.equal(test2, p.true))
print(all.equal(test3, cp.true))
# print(all.equal(test4, cp.true))

# # huge file.backed big.matrix
#
# X2 <- big.matrix(5000, 50000, backingfile = "tmp", backingpath = ".")
# for (i in 1:50) {
#   X2[, 1:1000 + (i - 1) * 1000] <- rnorm(length(X2) / 50)
# }
#
# y2 <- rnorm(ncol(X2))
#
# print(microbenchmark(
#   test3 <- armaProdVec(X2@address, y2, 1:5000 - 1),
#   test4 <- rcppProdVec(X2@address, y2),
#   times = 10
# ))

