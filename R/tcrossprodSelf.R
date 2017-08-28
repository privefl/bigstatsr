################################################################################

#' Tcrossprod
#'
#' Compute \eqn{X.row X.row^T} for a Filebacked Big Matrix `X`
#' after applying a particular scaling to it.
#'
#' This algorithm is not really memory efficient. I'm planning on fixing this,
#' if it is really needed.
#'
#' @inheritParams bigstatsr-package
#' @inheritSection bigstatsr-package Matrix parallelization
#'
#' @return A matrix, \eqn{X.row X.row^T}, with the following two attributes:
#' - a numeric vector `center` of column scaling,
#' - a numeric vector `scale` of column scaling.
#' @export
#' @seealso [tcrossprod]
#'
#' @example examples/example-tcrossprodSelf.R
#'
big_tcrossprodSelf <- function(X, fun.scaling,
                               ind.row = rows_along(X),
                               ind.col = cols_along(X),
                               block.size = block_size(nrow(X))) {

  check_args()

  n <- length(ind.row)
  K <- matrix(0, n, n)
  m <- length(ind.col)

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  means <- numeric(m)
  sds   <- numeric(m)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    ind.col.ind <- ind.col[ind]
    ms <- fun.scaling(X, ind.row = ind.row, ind.col = ind.col.ind)
    means[ind] <- ms$center
    sds[ind]   <- ms$scale
    tmp <- scaling(X[ind.row, ind.col.ind], ms$center, ms$scale)
    K <- incrSup2(K, tcrossprod(tmp))
  }

  # Complete the lower part of the symmetric matrix
  structure(complete2(K), center = means, scale = sds)
}

################################################################################
