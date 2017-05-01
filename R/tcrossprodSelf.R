################################################################################

#' Tcrossprod
#'
#' Compute \eqn{X.row X.row^T} for a `big.matrix` `X`
#' after applying a particular scaling to it.
#'
#' This algorithm is not really memory efficient. I'm planning on fixing this.
#'
#' @inheritParams bigstatsr-package
#'
#' @return A list of
#' - \eqn{K = X.row X.row^T},
#' - a numeric vector `mean` of column scaling,
#' - a numeric vector `sd` of column scaling.
#' @export
#' @seealso [tcrossprod]
#'
#' @example examples/example-tcrossprodSelf.R
big_tcrossprodSelf <- function(X., fun.scaling,
                               ind.row = rows_along(X.),
                               block.size = 1000) {

  check_args()

  X <- attach.BM(X.)
  n <- length(ind.row)
  K <- matrix(0, n, n)

  # means and sds of each column
  ms <- fun.scaling(X, ind.row = ind.row)

  intervals <- CutBySize(ncol(X), block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.row, ind], ms$mean[ind], ms$sd[ind])

    K <- incrSup2(K, tcrossprod(tmp))
  }

  # Complete the lower part of the symmetric matrix
  list(K = complete2(K), mean = ms$mean, sd = ms$sd)
}

################################################################################
