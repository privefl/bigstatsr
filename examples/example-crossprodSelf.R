X <- big_attachExtdata()

# Comparing with tcrossprod
big_noscale <- big_scale(center = FALSE)
K <- big_crossprodSelf(X, fun.scaling = big_noscale)
dim(K)

true <- crossprod(X[])
all.equal(K, true, check.attributes = FALSE)

# Using only half of the data
n <- nrow(X)
ind <- sort(sample(n, n/2))
K2 <- big_crossprodSelf(X, fun.scaling = big_noscale, ind.row = ind)
dim(K2)

true2 <- crossprod(X[ind, ])
all.equal(K2, true2, check.attributes = FALSE)


