# Simulating some data
X <- big.matrix(19, 43)
X[] <- rnorm(length(X))


# center and scale
vec.center <- colmeans(X)
vec.scale <- colsds(X)

# Comparing with tcrossprod
test <- BigXYt(X = X,
               block.size = 10,
               vec.center = vec.center,
               vec.scale = vec.scale)

mat <- sweep(sweep(X[,], 2, vec.center, '-'), 2, vec.scale, '/')
diff <- test[,] - tcrossprod(mat)
print(max(abs(diff)))


# Using only half of the data for "training"
ind <- sort(sample(nrow(X), nrow(X)/2))
vec.center <- colmeans(X, ind)
vec.scale <- colsds(X, ind)

test <- BigXYt(X = X,
               block.size = 10,
               ind.train = ind,
               vec.center = vec.center,
               vec.scale = vec.scale)

mat1 <- sweep(sweep(X[ind, ],  2, vec.center, '-'), 2, vec.scale, '/')
mat2 <- sweep(sweep(X[-ind, ], 2, vec.center, '-'), 2, vec.scale, '/')
diff1 <- test[[1]][,] - tcrossprod(mat1)
print(max(abs(diff1)))
diff2 <- test[[2]][,] - tcrossprod(mat2, mat1)
print(max(abs(diff2)))
