# Simulating some data
X <- big.matrix(17, 41)
X[] <- rnorm(length(X))

# Comparing with tcrossprod
big_noscale <- big_scale(center = FALSE)
test <- big_tcrossprodSelf(X, fun.scaling = big_noscale)
print(dim(test$K))
print(all.equal(test$K, tcrossprod(X[,])))

# Using only half of the data for "training"
ind <- sort(sample(nrow(X), nrow(X)/2))
test2 <- big_tcrossprodSelf(X, fun.scaling = big_noscale,
                            ind.train = ind)
print(dim(test2$K))
print(all.equal(test2$K, tcrossprod(X[ind, ])))
