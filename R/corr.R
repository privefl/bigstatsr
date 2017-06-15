################################################################################

#' Correlation
#'
#' Compute the correlation matrix of a `big.matrix`.
#'
#' This algorithm is not really memory efficient. I'm planning on fixing this,
#' if it is really needed.
#'
#' @inheritParams bigstatsr-package
#'
#' @return The correlation matrix
#' @export
#' @seealso [cor]
#'
#' @example examples/example-corr.R
big_cor <- function(X.,
                    ind.row = rows_along(X.),
                    ind.col = cols_along(X.),
                    block.size = 1000) {

  check_args()

  X <- attach.BM(X.)
  m <- length(ind.col)
  K <- matrix(NA_real_, m, m)

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind1 <- seq2(intervals[j, ])
    tmp1 <- X[ind.row, ind.col[ind1]]
    for (i in 1:j) {
      ind2 <- seq2(intervals[i, ])
      tmp2 <- X[ind.row, ind.col[ind2]]

      K[ind2, ind1] <- crossprod(tmp2, tmp1)
    }
  }

  # Complete the lower part of the symmetric matrix
  K <- complete2(K)

  # 'correlize' the cross-product (see https://goo.gl/HK2Bqb)
  sums <- big_colstats(X, ind.row, ind.col)$sum / sqrt(length(ind.row))
  diags <- sqrt(diag(K) - sums^2)
  correlize(K, shift = sums, scale = diags)
}

################################################################################
