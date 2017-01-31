################################################################################

#' Product between a "big.matrix" and a vector
#'
#' @inheritParams bigstatsr-package
#'
#' @return \eqn{X /cdot y}.
#' @export
#'
#' @examples
#' N <- 100
#' M <- 20
#' X <- big.matrix(N, M)
#' X[] <- rnorm(length(X))
#' y <- rnorm(M)
#'
#' test <- big_prodVec(X, y) # vector
#' true <- X[,] %*% y        # one-column matrix
#' print(all.equal(test, as.numeric(true)))
#'
#' # subsetting
#' ind.row <- sample(N, N/2)
#' ind.col <- sample(M, M/2)
#'
#' # test2 <- big_prodVec(X, y, ind.row, ind.col)
#' # returns an error. You need to use the subset of y:
#' test2 <- big_prodVec(X, y[ind.col], ind.row, ind.col)
#' true2 <- X[ind.row, ind.col] %*% y[ind.col]
#' print(all.equal(test2, as.numeric(true2)))
big_prodVec <- function(X, y, ind.row = seq(nrow(X)), ind.col = seq(ncol(X))) {
  pMatVec4(X@address, y, ind.row, ind.col)
}

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

# #' @export
# big_mult <- function(X, mat, ind.train, block.size, use.Eigen) {
#   if (!is.matrix(mat)) mat <- as.matrix(mat)
#   m <- ncol(X)
#   stopifnot(m == nrow(mat))
#   res <- matrix(0, length(ind.train), ncol(mat))
#
#   intervals <- CutBySize(m, block.size)
#   nb.block <- nrow(intervals)
#
#   for (j in 1:nb.block) {
#     ind <- seq2(intervals[j, ])
#     res <- incrMat(res, mult(X[ind.train, ind], mat[ind, ], use.Eigen))
#   }
#
#   res
# }
#
# #' @export
# big_multScaled <- function(X, mat, ind.train, block.size,
#                            vec.center, vec.scale, use.Eigen) {
#   if (!is.matrix(mat)) mat <- as.matrix(mat)
#   mat <- scaling2(mat, vec.scale)
#   tmp <- big_mult(X, mat, ind.train, block.size, use.Eigen)
#   tmp2 <- crossprod(vec.center, mat)
#   scaling3(tmp, as.numeric(tmp2))
# }

multScaled2 <- function(X, mat,
                        ind.train = seq(nrow(X)),
                        ind.col = seq(ncol(X)),
                        block.size = 1000,
                        vec.center = rep(0, m),
                        vec.scale = rep(1, m),
                        use.Eigen = TRUE) {
  m <- length(ind.col)
  stopifnot(m == nrow(mat))
  res <- matrix(0, length(ind.train), ncol(mat))

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.train, ind.col[ind]], vec.center[ind], vec.scale[ind])

    res <- incrMat(res, mult(tmp, mat[ind, ], use.Eigen))
  }

  res
}

################################################################################
