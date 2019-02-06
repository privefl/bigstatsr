X <- FBM(13, 17, init = rnorm(221))
true <- crossprod(X[])

# No scaling
K1 <- crossprod(X)
class(K1)
stopifnot(all.equal(K1, true))

K2 <- big_crossprodSelf(X)
class(K2)
K2$backingfile
stopifnot(all.equal(K2[], true))

microbenchmark::microbenchmark(
  crossprod(X[]), crossprod(X), big_crossprodSelf(X)
)

# big_crossprodSelf() provides some scaling and subsetting
# Example using only half of the data:
n <- nrow(X)
ind <- sort(sample(n, n/2))
K3 <- big_crossprodSelf(X, fun.scaling = big_scale(), ind.row = ind)
true2 <- crossprod(scale(X[ind, ]))
stopifnot(all.equal(K3[], true2))
