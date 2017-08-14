X <- new_FBM(1e3, 5e3, init = rnorm(5e6))
X[, 1]
# X2 <- big.matrix(1e3, 5e3, init = rnorm(5e6))
# X2[, 1]

ind.row <- rows_along(X)
ind.col <- 1:500

# tmp <- big_apply2(ind = seq_along(ind.col), a.FUN = function(X, X2, ind) {
#   X2[, ind] <- X[ind.row, ind.col[ind]]
#   NULL
# }, block.size = 100, X = X, X2 = X2)
#
# all.equal(X2[], X[ind.row, ind.col])

# (8 * n * m) / 1024^3 < opt
# m < opt / (8 * n / 1024^3)
block_size <- function(n) {
  max(1, floor(getOption("bigstatsr.block.sizeGB") / (8 * n / 1024^3)))
}

big_copy <- function(X, ind.row = rows_along(X),
                     ind.col = cols_along(X),
                     type = X$description$type,
                     ...,
                     block.size = block_size(length(ind.row)),
                     ncores = 1) {

  X2 <- new_FBM(
    nrow = length(ind.row),
    ncol = length(ind.col),
    type = type,
    init = NULL,
    ...
  )
  print(type)
  print(block.size)
  big_apply2(ind = seq_along(ind.col), a.FUN = function(X, X2, ind,
                                                        ind.row, ind.col) {
    X2[, ind] <- X[ind.row, ind.col[ind]]
    NULL
  }, block.size = force(block.size), ncores = ncores,
  X = X, X2 = X2, ind.row = ind.row, ind.col = ind.col)

  X2
}

test <- big_copy(X, ind.row, ind.col)
all.equal(test[], X[ind.row, ind.col])
