#' @export
produ <- function(X, x) produ3(X@address, x)
#' @export
cprodu <- function(X, x) crossprodu3(X@address, x)

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

#' @export
big_mult <- function(X, mat, ind.train, block.size, use.Eigen) {
  if (!is.matrix(mat)) mat <- as.matrix(mat)
  m <- ncol(X)
  stopifnot(m == nrow(mat))
  res <- matrix(0, length(ind.train), ncol(mat))

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    res <- incrMat(res, mult(X[ind.train, ind], mat[ind, ], use.Eigen))
  }

  res
}

#' @export
big_multScaled <- function(X, mat, ind.train, block.size,
                           vec.center, vec.scale, use.Eigen) {
  if (!is.matrix(mat)) mat <- as.matrix(mat)
  mat <- scaling2(mat, vec.scale)
  tmp <- big_mult(X, mat, ind.train, block.size, use.Eigen)
  tmp2 <- crossprod(vec.center, mat)
  scaling3(tmp, as.numeric(tmp2))
}
