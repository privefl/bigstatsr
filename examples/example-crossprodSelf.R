# Simulating some data
X <- big.matrix(17, 41)
X[] <- rnorm(length(X))

# Comparing with tcrossprod
test <- big_crossprodSelf(X, fun.scaling = big_noscale)
print(all.equal(test[,], crossprod(X[,])))

# Using only half of the data for "training"
ind <- sort(sample(nrow(X), nrow(X)/2))
test <- big_crossprodSelf(X, fun.scaling = big_noscale,
                          ind.train = ind)
print(all.equal(test[,], crossprod(X[ind, ])))
