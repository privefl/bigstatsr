X <- big_attachExtdata()

# Comparing with tcrossprod
big_noscale <- big_scale(center = FALSE)
K <- big_tcrossprodSelf(X, fun.scaling = big_noscale)
class(K)
dim(K)
K$backingfile

true <- tcrossprod(X[])
all.equal(K[], true)

# Using only half of the data
n <- nrow(X)
ind <- sort(sample(n, n/2))
K2 <- big_tcrossprodSelf(X, fun.scaling = big_noscale, ind.row = ind)

true2 <- tcrossprod(X[ind, ])
all.equal(K2[], true2)
