set.seed(1)

X.desc <- big_attachExtdata()
n <- nrow(X.desc)

# Using only half of the data
ind <- sort(sample(n, n/2))

test <- big_SVD(X. = X.desc, fun.scaling = big_scale(), ind.row = ind)
str(test)
plot(test$u)

pca <- prcomp(attach.BM(X.desc)[ind, ], center = TRUE, scale. = TRUE)

# same scaling
all.equal(test$means, pca$center)
all.equal(test$sds, pca$scale)

# scores and loadings are the same or opposite
# except for last eigenvalue which is equal to 0
# due to centering of columns
scores <- test$u %*% diag(test$d)
class(test)
scores2 <- predict(test) # use this function to predict scores
all.equal(scores, scores2)
dim(scores)
dim(pca$x)
tail(pca$sdev)
plot(scores2, pca$x[, 1:ncol(scores2)])
plot(test$v[1:100, ], pca$rotation[1:100, 1:ncol(scores2)])

# projecting on new data
X2 <- sweep(sweep(attach.BM(X.desc)[-ind, ], 2, test$means, '-'),
            2, test$sds, '/')
scores.test <- X2 %*% test$v
ind2 <- setdiff(rows_along(X.desc), ind)
scores.test2 <- predict(test, X.desc, ind.row = ind2) # use this
all.equal(scores.test, scores.test2)
scores.test3 <- predict(pca, attach.BM(X.desc)[-ind, ])
plot(scores.test2, scores.test3[, 1:ncol(scores.test2)])
