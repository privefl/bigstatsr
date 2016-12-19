# Simulating some data
X <- big.matrix(73, 43)
X[] <- rnorm(length(X))

# Using only half of the data for "training"
ind <- sort(sample(nrow(X), nrow(X)/2))

test <- big_SVD(X = X,
                 fun.scaling = big_scale(),
                 ind.train = ind)
str(test)

pca <- prcomp(X[ind, ], center = TRUE, scale. = TRUE)

# same scaling
print(all.equal(test$means, pca$center))
print(all.equal(test$sds, pca$scale))

# scores and loadings are the same or opposite
# except for last eigenvalue which is equal to 0
# due to centering of columns
scores <- test$u %*% diag(test$d)
scores2 <- big_predScoresPCA(test) # use this function to predict scores
print(all.equal(scores, scores2))
print(dim(scores))
print(dim(pca$x))
print(tail(pca$sdev))
plot(scores2, pca$x[, 1:ncol(scores2)])
plot(test$v, pca$rotation[, 1:ncol(scores2)])

# projecting on new data
X.test <- sweep(sweep(X[-ind, ], 2, test$means, '-'), 2, test$sds, '/')
scores.test <- X.test %*% test$v
ind2 <- setdiff(seq(nrow(X)), ind)
scores.test2 <- big_predScoresPCA(test, X, ind.test = ind2) # use this
print(all.equal(scores.test, scores.test2))
scores.test3 <- predict(pca, X[-ind, ])
plot(scores.test2, scores.test3[, 1:ncol(scores.test2)])
