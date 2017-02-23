X.desc <- big_attachExtdata()

# get the means of each column
colMeans_sub <- function(X, ind) colMeans(X[, ind])
colmeans <- big_apply(X.desc, a.FUN = colMeans_sub, a.combine = 'c')

# get the norms of each column
colNorms_sub <- function(X, ind) sqrt(colSums(X[, ind]^2))
colnorms <- big_apply(X.desc, colNorms_sub, a.combine = 'c')

# get the sums of each row
# split along rows: need to change the "complete" `ind` parameter
rowsums <- big_apply(X.desc, a.FUN = function(X, ind) rowSums(X[ind, ]),
                     ind = rows_along(X.desc), a.combine = 'c',
                     block.size = 100)
# it is usually preferred to split along columns
# because `big.matrix` are stored by column.
rowsums2 <- big_apply(X.desc, a.FUN = function(X, ind) rowSums(X[, ind]),
                      a.combine = '+')

## Every extra parameter to `a.FUN` should be passed to `big_apply`
# get the crossproduct between X and a matrix A
A <- matrix(0, nrow(X.desc), 10)
A[] <- rnorm(length(A))
XtA <- big_apply(X.desc, function(X, ind, mat) crossprod(X[, ind], mat),
                 a.combine = 'rbind', mat = A)

# get the product between X and a matrix B
B <- matrix(0, ncol(X.desc), 10)
B[] <- rnorm(length(B))
XB <- big_apply(X.desc, function(X, ind, mat) X[, ind] %*% mat[ind, ],
                a.combine = '+', mat = B)
