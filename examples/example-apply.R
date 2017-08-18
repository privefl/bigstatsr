X <- big_attachExtdata()

# get the means of each column
colMeans_sub <- function(X, ind) colMeans(X[, ind])
str(colmeans <- big_apply(X, a.FUN = colMeans_sub, a.combine = 'c'))

# get the norms of each column
colNorms_sub <- function(X, ind) sqrt(colSums(X[, ind]^2))
str(colnorms <- big_apply(X, colNorms_sub, a.combine = 'c'))

# get the sums of each row
# split along rows: need to change the "complete" `ind` parameter
str(rowsums <- big_apply(X, a.FUN = function(X, ind) rowSums(X[ind, ]),
                         ind = rows_along(X), a.combine = 'c',
                         block.size = 100))
# it is usually preferred to split along columns
# because `big.matrix` are stored by column.
str(rowsums2 <- big_apply(X, a.FUN = function(X, ind) rowSums(X[, ind]),
                          a.combine = '+'))

## Every extra parameter to `a.FUN` should be passed to `big_apply`
# get the crossproduct between X and a matrix A
# note that we don't explicitly pass `ind.col` to `a.FUN`
body(big_cprodMat)

# get the product between X and a matrix B
# here, we must explicitly pass `ind.col` to `a.FUN`
# because the right matrix also needs to be subsetted.
body(big_prodMat)
