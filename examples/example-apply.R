# simulate some data
N <- 50
M <- 5000
X <- big.matrix(N, M)
X[] <- rnorm(length(X))

# get the means of each column
colmeans <- big_apply(X, colMeans, .combine = 'c')
# is the shorter version of
colmeans2 <- big_apply(X, function(X, ind) {
  mat <- X[, ind, drop = FALSE]
  colMeans(mat)
}, .combine = 'c', ind.arg = TRUE)
print(all.equal(colmeans, colmeans2))

# get the norms of each column
colnorms <- big_apply(X, function(mat) sqrt(colSums(mat^2)), .combine = 'c')

# get the sums of each row
rowsums <- big_apply(X, rowSums, .combine = '+')

# get the maximum element of X (in absolute value)
maxabs <- max(big_apply(X, function(x) max(abs(x)), .combine = 'c'))

# get the crossproduct between X and a matrix A
A <- matrix(0, N, 10)
A[] <- rnorm(length(A))
XtA <- big_apply(X, function(x) crossprod(x, A), .combine = 'rbind')

# get the product between X and a matrix B
B <- matrix(0, M, 10)
B[] <- rnorm(length(B))
XB <- big_apply(X, function(x, ind, A) {
  x.part <- x[, ind, drop = FALSE]
  A.part <- A[ind, , drop = FALSE]
  x.part %*% A.part
}, .combine = '+', ind.arg = TRUE, A = B)
print(all.equal(XB, X[,] %*% B))
