# Simulating some data
X <- big.matrix(17, 41, shared = FALSE)
X[] <- rnorm(length(X))


# center and scale
vec.center <- colmeans(X)
vec.scale <- colsds(X)

# Comparing with tcrossprod
test <- BigXtX(X = X,
               block.size = 10,
               vec.center = vec.center,
               vec.scale = vec.scale)

mat <- sweep(sweep(X[,], 2, vec.center, '-'), 2, vec.scale, '/')
diff <- test[,] - crossprod(mat)
print(max(abs(diff)))


# Using only half of the data for "training"
ind <- sort(sample(nrow(X), nrow(X)/2))
vec.center <- colmeans(X, ind)
vec.scale <- colsds(X, ind)

test <- BigXtX(X = X,
               block.size = 10,
               ind.train = ind,
               vec.center = vec.center,
               vec.scale = vec.scale)

mat <- sweep(sweep(X[ind, ],  2, vec.center, '-'), 2, vec.scale, '/')
diff <- test[,] - crossprod(mat)
print(max(abs(diff)))
