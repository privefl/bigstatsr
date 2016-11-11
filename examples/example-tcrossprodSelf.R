# Simulating some data
X <- big.matrix(17, 41)
X[] <- rnorm(length(X))

# Comparing with tcrossprod
test <- big_tcrossprodSelf(X, fun.scaling = big_noscale)
print(dim(test))
print(all.equal(test[,], tcrossprod(X[,])))

# Using only half of the data for "training"
ind <- sort(sample(nrow(X), nrow(X)/2))
test2 <- big_tcrossprodSelf(X, fun.scaling = big_noscale,
                            ind.train = ind)
print(dim(test2))
print(all.equal(test2[,], tcrossprod(X[ind, ])))
