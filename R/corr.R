################################################################################

#' Correlation
#'
#' Compute the correlation matrix of a Filebacked Big Matrix.
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
#'
big_cor <- function(X,
                    ind.row = rows_along(X),
                    ind.col = cols_along(X),
                    block.size = block_size(nrow(X))) {

  check_args()

  m <- length(ind.col)
  K <- matrix(NA_real_, m, m)

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  sums <- numeric(m)

  for (j in seq_len(nb.block)) {
    ind1 <- seq2(intervals[j, ])
    tmp1 <- X[ind.row, ind.col[ind1]]
    sums[ind1] <- colSums(tmp1)
    K[ind1, ind1] <- crossprod(tmp1)
    for (i in seq_len(j - 1)) {
      ind2 <- seq2(intervals[i, ])
      tmp2 <- X[ind.row, ind.col[ind2]]
      K.part <- crossprod(tmp2, tmp1)
      K[ind2, ind1] <- K.part
      K[ind1, ind2] <- t(K.part)
    }
  }

  # 'Correlize' the cross-product (see https://goo.gl/HK2Bqb)
  sums <- sums / sqrt(length(ind.row))
  diags <- sqrt(diag(K) - sums^2)
  correlize(K, shift = sums, scale = diags)
}

################################################################################
