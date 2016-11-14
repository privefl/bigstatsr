# Simulating some data
X <- big.matrix(73, 43)
X[] <- rnorm(length(X))


# Comparing with prcomp
test <- big_SVD(X = X,
                fun.scaling = big_scale(),
                block.size = 10)
str(test)

pca <- prcomp(X[,], center = TRUE, scale. = TRUE)

# same scaling
print(all.equal(test$means, pca$center))
print(all.equal(test$sds, pca$scale))

# scores and loadings are the same or opposite
scores <- test$u %*% diag(test$d)
print(dim(scores))
print(dim(pca$x))
plot(as.numeric(scores), as.numeric(pca$x))
plot(as.numeric(test$v), as.numeric(pca$rotation))

# reconstruction
print(all.equal(tcrossprod(scores, test$v), scale(X[,]),
                check.attributes = FALSE))


# Using only half of the data for "training"
ind <- sort(sample(nrow(X), nrow(X)/2))

test2 <- big_SVD(X = X,
                 fun.scaling = big_scale(),
                 ind.train = ind,
                 block.size = 10)
str(test2)

pca2 <- prcomp(X[ind, ], center = TRUE, scale. = TRUE)

# same scaling
print(all.equal(test2$means, pca2$center))
print(all.equal(test2$sds, pca2$scale))

# scores and loadings are the same or opposite
# except for last eigenvalue which is equal to 0
# due to centering of columns
scores2 <- test2$u %*% diag(test2$d)
print(dim(scores2))
print(dim(pca2$x))
print(tail(pca2$sdev))
plot(as.numeric(scores2), as.numeric(pca2$x[, 1:ncol(scores2)]))
plot(as.numeric(test2$v), as.numeric(pca2$rotation[, 1:ncol(scores2)]))
