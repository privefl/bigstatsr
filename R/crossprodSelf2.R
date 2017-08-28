################################################################################

#' Crossprod
#'
#' Compute \eqn{X.row^T X.row} for a Filebacked Big Matrix `X`
#' after applying a particular scaling to it.
#'
#' This algorithm is not really memory efficient. I'm planning on fixing this,
#' if it is really needed.
#'
#' @inheritParams bigstatsr-package
#' @inheritSection bigstatsr-package Matrix parallelization
#'
#' @return A matrix, \eqn{X.row^T X.row}, with the following two attributes:
#' - a numeric vector `mean` of column scaling,
#' - a numeric vector `sd` of column scaling.
#' @export
#' @seealso [crossprod]
#'
#' @example examples/example-crossprodSelf.R
#'
big_crossprodSelf2 <- function(X,
                               fun.scaling,
                               ind.row = rows_along(X),
                               ind.col = cols_along(X),
                               block.size = block_size(nrow(X))) {

  check_args()

  m <- length(ind.col)
  K <- matrix(NA_real_, m, m)

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  mu    <- numeric(m)
  delta <- numeric(m)
  sums  <- numeric(m)

  for (j in seq_len(nb.block)) {

    ind1 <- seq2(intervals[j, ])
    tmp1 <- X[ind.row, ind.col[ind1]]

    ms <- fun.scaling(X, ind.row = ind.row, ind.col = ind.col[ind1])
    mu[ind1]    <- ms$mean
    delta[ind1] <- ms$sd
    sums[ind1]  <- colSums(tmp1)

    K[ind1, ind1] <- crossprod(tmp1)
    for (i in seq_len(j - 1)) {
      ind2 <- seq2(intervals[i, ])
      tmp2 <- X[ind.row, ind.col[ind2]]

      K.part <- crossprod(tmp2, tmp1)
      K[ind2, ind1] <- K.part
      K[ind1, ind2] <- t(K.part)
    }
  }

  structure(K, mean = ms$mean, sd = ms$sd)
}

################################################################################
