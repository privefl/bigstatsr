set.seed(1)

X.desc <- big_attachExtdata()
K <- 10

# Using only half of the data for "training"
n <- nrow(X.desc)
ind <- sort(sample(n, n/2))
test <- big_randomSVD(X.desc, fun.scaling = big_scale(), ind.row = ind, k = K)
str(test)
plot(test$u)

pca <- prcomp(attach.BM(X.desc)[ind, ], center = TRUE, scale. = TRUE)

# same scaling
all.equal(test$means, pca$center)
all.equal(test$sds, pca$scale)

# use this function to predict scores
class(test)
scores <- predict(test)
# scores and loadings are the same or opposite
plot(scores, pca$x[, 1:K])
plot(test$v, pca$rotation[, 1:K])

# projecting on new data
ind2 <- setdiff(rows_along(X.desc), ind)
scores.test2 <- predict(test, X.desc, ind.row = ind2)
scores.test3 <- predict(pca, attach.BM(X.desc)[-ind, ])
plot(scores.test2, scores.test3[, 1:K])

