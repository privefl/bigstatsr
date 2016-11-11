# Simulating some data
X <- big.matrix(1023, 511)
X[] <- rnorm(length(X))

# Comparing with prcomp
test <- big_randomSVD(X = X,
                      fun.scaling = big_scale(),
                      block.size = 50,
                      ncores = 1)

pca <- prcomp(X[,], center = TRUE, scale. = TRUE)

# same scaling
print(all.equal(test$means, pca$center))
print(all.equal(test$sds, pca$scale))

# scores and loadings are the same or opposite
scores <- test$u %*% diag(test$d)
plot(as.numeric(scores), as.numeric(pca$x[, 1:10]))
plot(as.numeric(test$v), as.numeric(pca$rotation[, 1:10]))


# Using only half of the data for "training"
ind <- sort(sample(nrow(X), nrow(X)/2))

test2 <- big_randomSVD(X = X, ind.train = ind,
                       fun.scaling = big_scale(),
                       block.size = 10, ncores = 1)

pca2 <- prcomp(X[ind, ], center = TRUE, scale. = TRUE)

# same scaling
print(all.equal(test2$means, pca2$center))
print(all.equal(test2$sds, pca2$scale))

# scores and loadings are the same or opposite
scores2 <- test2$u %*% diag(test2$d)
plot(as.numeric(scores2), as.numeric(pca2$x[, 1:10]))
plot(as.numeric(test2$v), as.numeric(pca2$rotation[, 1:10]))

