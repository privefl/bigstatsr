################################################################################

multScaled <- function(X, mat, ind.train, block.size,
                       vec.center, vec.scale, use.Eigen) {
  res <- matrix(0, length(ind.train), ncol(mat))

  intervals <- CutBySize(ncol(X), block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.train, ind], vec.center[ind], vec.scale[ind])

    res <- `if`(use.Eigen,
                incrMat(res, multEigen(tmp, mat[ind, ])),
                incrMat(res, tmp %*% mat[ind, ]))
  }

  res
}

################################################################################

crossprodScaled <- function(X, mat, ind.train, block.size,
                            vec.center, vec.scale, use.Eigen) {
  stopifnot(nrow(mat) == length(ind.train))
  m <- ncol(X)
  res <- matrix(0, m, ncol(mat))

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.train, ind], vec.center[ind], vec.scale[ind])

    res[ind, ] <- `if`(use.Eigen,
                       crossprodEigen5(tmp, mat),
                       crossprod(tmp, mat))
  }

  res
}

################################################################################
