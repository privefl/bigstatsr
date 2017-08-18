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
#'
#' @return A matrix, \eqn{X.row X.row^T}, with the following two attributes:
#' - a numeric vector `mean` of column scaling,
#' - a numeric vector `sd` of column scaling.
#' @export
#' @seealso [tcrossprod]
#'
#' @example examples/example-tcrossprodSelf.R
#'
big_tcrossprodSelf <- function(X, fun.scaling,
                               ind.row = rows_along(X),
                               ind.col = cols_along(X),
                               block.size = 1000) {

  check_args()

  n <- length(ind.row)
  K <- matrix(0, n, n)

  # means and sds of each column
  ms <- fun.scaling(X, ind.row = ind.row, ind.col = ind.col)

  intervals <- CutBySize(length(ind.col), block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.row, ind.col[ind]], ms$mean[ind], ms$sd[ind])

    K <- incrSup2(K, tcrossprod(tmp))
  }

  # Complete the lower part of the symmetric matrix
  structure(complete2(K), mean = ms$mean, sd = ms$sd)
}

################################################################################
