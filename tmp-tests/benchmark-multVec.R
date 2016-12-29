Rcpp::sourceCpp('tmp-tests/test-fastMatVec3.cpp')
require(bigmemory)

X <- big.matrix(5000, 5000, shared = FALSE)
X[] <- rnorm(length(X))

y <- rnorm(ncol(X))

require(microbenchmark)
print(microbenchmark(
  test <- armaProdVec(X@address, y, 1:5000 - 1),
  test2 <- rcppProdVec(X@address, y),
  times = 10
))

# huge file.backed big.matrix

X2 <- big.matrix(5000, 50000, backingfile = "tmp", backingpath = ".")
for (i in 1:50) {
  X2[, 1:1000 + (i - 1) * 1000] <- rnorm(length(X2) / 50)
}

y2 <- rnorm(ncol(X2))

print(microbenchmark(
  test3 <- armaProdVec(X2@address, y2, 1:5000 - 1),
  test4 <- rcppProdVec(X2@address, y2),
  times = 10
))

