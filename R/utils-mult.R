################################################################################

mult <- function(A, B, use.Eigen) {
  `if`(use.Eigen, multEigen(A, B), A %*% B)
}

################################################################################

cross <- function(A, B, use.Eigen) {
  `if`(use.Eigen, crossprodEigen5(A, B), crossprod(A, B))
}

################################################################################

multScaled <- function(X, mat, ind.train, block.size,
                       vec.center, vec.scale, use.Eigen) {
  m <- ncol(X)
  stopifnot(m == nrow(mat))
  res <- matrix(0, length(ind.train), ncol(mat))

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.train, ind], vec.center[ind], vec.scale[ind])

    res <- incrMat(res, mult(tmp, mat[ind, ], use.Eigen))
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

    res[ind, ] <- cross(tmp, mat, use.Eigen)
  }

  res
}

################################################################################
