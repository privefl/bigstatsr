#' Crossprod
#'
#' Compute \eqn{X.row^T X.row} for a `big.matrix` `X`
#' after applying a particular scaling to it.
#'
#' @inherit bigstatsr-package params details
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
                              block.size = 1000,
                              use.Eigen = !detect_MRO()) {
  X <- attach.BM(X.)
  m <- ncol(X)
  K <- matrix(NA_real_, m, m)

  means_sds <- fun.scaling(X, ind.row)

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind1 <- seq2(intervals[j, ])
    tmp1 <- scaling(X[ind.row, ind1],
                    means_sds$mean[ind1],
                    means_sds$sd[ind1])
    for (i in 1:j) {
      ind2 <- seq2(intervals[i, ])
      tmp2 <- scaling(X[ind.row, ind2],
                      means_sds$mean[ind2],
                      means_sds$sd[ind2])

      K[ind2, ind1] <- cross(tmp2, tmp1, use.Eigen)
    }
  }

  # Complete the lower part of the symmetric matrix
  list(K =  complete2(K), mean = means_sds$mean, sd = means_sds$sd)
}
