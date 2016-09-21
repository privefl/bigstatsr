multScaled <- function(X, block.size, mat, vec.center, vec.scale) {

  intervals <- CutBySize(ncol(X), block.size)
  nb.block <- nrow(intervals)

  res <- matrix(0, nrow(X), ncol(mat))

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[, ind], vec.center[ind], vec.scale[ind])
    res <- incrMat(res, tmp %*% mat[ind, ])
  }

  return(res)
}
