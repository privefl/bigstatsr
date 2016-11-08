#' @name BigXtX
#' @description Compute \eqn{X^T X} for a "big.matrix" X
#' after applying a particular scaling to it.
#' @title Crossprod for a big.matrix.
#' @inheritParams BigXYt
#' @param use.Eigen Use the \code{Eigen} library to compute
#' \eqn{X^T X}? \code{TRUE} is the default.
#' @seealso [crossprod]
#' @return \eqn{X.train^T X.train}
#' @export
#' @example examples/example.BigXtX.R

#' Crossprod for a "big.matrix".
#'
#' Compute \eqn{X^T X} for a "big.matrix" X
#' after applying a particular scaling to it.
#'
#' @inherit params details
#' @param vec.center
#' @param vec.scale
#' @param use.Eigen
#' @param progress
#'
#' @return
#' @export
#'
#' @examples
BigXtX <- function(X,
                   block.size,
                   ind.train = seq(nrow(X)),
                   fun.scaling,
                   use.Eigen = TRUE,
                   progress = TRUE) {
  check_X(X)

  progress <- progress & interactive()
  m <- ncol(X)

  bigK <- big.matrix(m, m, type = "double", shared = FALSE)

  # function to compute X^T*X
  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  if (progress) {
    MAX <- nb.block*(nb.block + 1)/2
    pb <- utils::txtProgressBar(min = 0, max = MAX, style = 3)
  }

  for (j in 1:nb.block) {
    ind1 <- seq2(intervals[j, ])
    tmp1 <- scaling(X[ind.train, ind1], vec.center[ind1], vec.scale[ind1])
    for (i in 1:j) {
      if (progress) utils::setTxtProgressBar(pb, j*(j - 1)/2 + (i - 1))
      ind2 <- seq2(intervals[i, ])
      tmp2 <- scaling(X[ind.train, ind2], vec.center[ind2], vec.scale[ind2])
      if (use.Eigen) {
        bigK[ind2, ind1] <- crossprodEigen5(tmp2, tmp1)
      } else {
        bigK[ind2, ind1] <- crossprod(tmp2, tmp1)
      }
    }
  }

  # Complete the lower part of the symmetric matrix
  complete(bigK@address)

  if (progress) {
    utils::setTxtProgressBar(pb, MAX)
    close(pb)
  }

  return(bigK)
}
