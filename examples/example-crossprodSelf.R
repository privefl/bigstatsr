X <- FBM(13, 17, init = rnorm(221))

# Comparing with tcrossprod
big_noscale <- big_scale(center = FALSE)
K <- big_crossprodSelf(X, fun.scaling = big_noscale)
class(K)
dim(K)
K$backingfile

true <- crossprod(X[])
all.equal(K[], true)

# Using only half of the data
n <- nrow(X)
ind <- sort(sample(n, n/2))
K2 <- big_crossprodSelf(X, fun.scaling = big_noscale, ind.row = ind)

true2 <- crossprod(X[ind, ])
all.equal(K2[], true2)


