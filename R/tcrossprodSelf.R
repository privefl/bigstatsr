#' Tcrossprod
#'
#' Compute \eqn{X.row X.row^T} for a `big.matrix` `X`
#' after applying a particular scaling to it.
#'
#' @inherit bigstatsr-package params details
#'
#' @return A list of
#' - \eqn{K = X.row X.row^T},
#' - a numeric vector `mean` of column scaling,
#' - a numeric vector `sd` of column scaling.
#' @export
#' @seealso [tcrossprod]
#'
#' @example examples/example-tcrossprodSelf.R
big_tcrossprodSelf <- function(X.,
                               fun.scaling,
                               ind.row = rows_along(X.),
                               block.size = 1000,
                               use.Eigen = !detect_MRO()) {
  X <- attach.BM(X.)
  n <- length(ind.row)
  K <- matrix(0, n, n)

  means_sds <- fun.scaling(X, ind.row = ind.row)

  intervals <- CutBySize(ncol(X), block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.row, ind],
                   means_sds$mean[ind],
                   means_sds$sd[ind])
    if (use.Eigen) {
      tcrossprodEigen3(K, tmp)
    } else {
      K <- incrSup2(K, tcrossprod(tmp))
    }
  }

  # Complete the lower part of the symmetric matrix
  list(K = complete2(K), mean = means_sds$mean, sd = means_sds$sd)
}
