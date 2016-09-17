diffPCs <- function(test, rot) {
  k <- ncol(test)
  diff1 <- test - rot[, 1:k]
  diff2 <- test + rot[, 1:k]
  diff <- pmin(abs(diff1), abs(diff2))
  max(diff)
}

# Simulating some data
X <- big.matrix(50, 100)
X[] <- rnorm(length(X))


# center and scale
vec.center <- colmeans(X)
vec.scale <- colsds(X)

# Comparing with prcomp
test <- DualBigPCA(X = X,
                   block.size = 10,
                   vec.center = vec.center,
                   vec.scale = vec.scale)

pca <- prcomp(X[,], center = TRUE, scale. = TRUE)

# Note the difference in dimensions
print(dim(test))
print(dim(pca$x))
plot(pca$sdev)
print(pca$sdev[50])

# PCs are the same or opposite
print(diffPCs(test, pca$x))


# Using only half of the data for "training"
ind <- sort(sample(nrow(X), nrow(X)/2))
vec.center <- colmeans(X, ind)
vec.scale <- colsds(X, ind)

test <- DualBigPCA(X = X,
               block.size = 10,
               ind.train = ind,
               vec.center = vec.center,
               vec.scale = vec.scale)

pca <- prcomp(X[ind, ], center = TRUE, scale. = TRUE)
print(diffPCs(test[ind, ], pca$x))
pred <- predict(pca, X[-ind, ])
print(diffPCs(test[-ind, ], pred))
