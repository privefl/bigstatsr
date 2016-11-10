#' Crossprod for a big.matrix.
#'
#' Compute \eqn{X.train^T X.train} for a `big.matrix` X
#' after applying a particular scaling to it.
#'
#' @inherit bigstatsr-package params details
#' @inheritDotParams bigmemory::big.matrix -nrow -ncol -type -init
#'
#' @return \eqn{X.train^T X.train} as a `big.matrix`. Its dimension
#' is ncol(X) x ncol(X) and its type is `double`.
#' @export
#'
#' @example examples/example-crossprodSelf.R
big_crossprodSelf <- function(X,
                              fun.scaling,
                              ind.train = seq(nrow(X)),
                              block.size = 1000,
                              use.Eigen = TRUE,
                              ...) {
  check_X(X)

  m <- ncol(X)
  bigK <- big.matrix(m, m, type = "double", ...)

  # function to compute X^T*X
  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  means_sds <- fun.scaling(X, ind.train)

  for (j in 1:nb.block) {
    ind1 <- seq2(intervals[j, ])
    tmp1 <- scaling(X[ind.train, ind1],
                    means_sds$mean[ind1],
                    means_sds$sd[ind1])
    for (i in 1:j) {
      ind2 <- seq2(intervals[i, ])
      tmp2 <- scaling(X[ind.train, ind2],
                      means_sds$mean[ind2],
                      means_sds$sd[ind2])

      bigK[ind2, ind1] <- `if`(use.Eigen,
                               crossprodEigen5(tmp2, tmp1),
                               crossprod(tmp2, tmp1))
    }
  }

  # Complete the lower part of the symmetric big.matrix
  complete(bigK@address)

  bigK
}
