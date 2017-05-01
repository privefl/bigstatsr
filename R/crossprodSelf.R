################################################################################

#' Crossprod
#'
#' Compute \eqn{X.row^T X.row} for a `big.matrix` `X`
#' after applying a particular scaling to it.
#'
#' This algorithm is not really memory efficient. I'm planning on fixing this.
#'
#' @inheritParams bigstatsr-package
#'
#' @return A list of
#' - \eqn{K = X.row^T X.row},
#' - a numeric vector `mean` of column scaling,
#' - a numeric vector `sd` of column scaling.
#' @export
#' @seealso [crossprod]
#'
#' @example examples/example-crossprodSelf.R
big_crossprodSelf <- function(X.,
                              fun.scaling,
                              ind.row = rows_along(X.),
                              block.size = 1000) {

  check_args()

  X <- attach.BM(X.)
  m <- ncol(X)
  K <- matrix(NA_real_, m, m)

  # means and sds of each column
  ms <- fun.scaling(X, ind.row)

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind1 <- seq2(intervals[j, ])
    tmp1 <- scaling(X[ind.row, ind1], ms$mean[ind1], ms$sd[ind1])
    for (i in 1:j) {
      ind2 <- seq2(intervals[i, ])
      tmp2 <- scaling(X[ind.row, ind2], ms$mean[ind2], ms$sd[ind2])

      K[ind2, ind1] <- crossprod(tmp2, tmp1)
    }
  }

  # Complete the lower part of the symmetric matrix
  list(K =  complete2(K), mean = ms$mean, sd = ms$sd)
}

################################################################################
