#' Tcrossprod
#'
#' Compute \eqn{X.train X.train^T} for a `big.matrix` `X`
#' after applying a particular scaling to it.
#'
#' @inherit bigstatsr-package params details
#' @inheritDotParams bigmemory::big.matrix -nrow -ncol -type -init
#'
#' @return \eqn{X.train X.train^T} as a `big.matrix`. Its dimensions
#' are both `length(ind.train)` and its type is `double`.
#' @export
#' @seealso [tcrossprod]
#'
#' @example examples/example-tcrossprodSelf.R
big_tcrossprodSelf <- function(X,
                               fun.scaling,
                               ind.train = seq(nrow(X)),
                               block.size = 1000,
                               use.Eigen = TRUE,
                               ...) {
  check_X(X)

  n <- length(ind.train)
  bigK <- big.matrix(n, n, type = "double", init = 0, ...)

  # function to compute X*X^T
  intervals <- CutBySize(ncol(X), block.size)
  nb.block <- nrow(intervals)

  means_sds <- fun.scaling(X, ind.train)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.train, ind],
                   means_sds$mean[ind],
                   means_sds$sd[ind])
    if (use.Eigen) {
      tcrossprodEigen(bigK@address, tmp)
    } else {
      incrSup(bigK@address, tcrossprod(tmp))
    }
  }

  # Complete the lower part of the symmetric matrix
  complete(bigK@address)

  bigK
}
