#' @name BigXYt
#' @description Compute linear kernel matrices for a "big.matrix"
#' after applying a particular scaling to it.
#' @title Linear kernel matrices for a big.matrix.
#' @inheritParams bigstatsr-package
#' @param block.size Maximum number of columns read at once.
#' @param vec.center Vector that will be subtracted to the matrix,
#' columnwise. Typically, the mean of each column.
#' See \code{\link{colmeans}}.
#' @param vec.scale Vector that will be divided to the matrix
#' (after the substraction), columnwise.
#' Typically, the sd of each column. See \code{\link{colsds}}.
#' @param progress Use a progress bar for the computation
#' of the correlation matrix? Default is \code{TRUE}.
#' @param use.Eigen Use the \code{Eigen} library to compute
#' \eqn{X X^T}? \code{TRUE} is the default.
#' If \code{FALSE}, use \code{R}'s \code{tcrossprod}. See details.
#' @details To compute \eqn{X X^T}, using \code{Eigen} library is faster.
#' However, if you link \code{R} with an optimized math library,
#' using \code{R}'s \code{tcrossprod} can be faster.
#'
#' For example, you can easily link \code{R} with the
#' \href{https://software.intel.com/en-us/intel-mkl}{Intel®
#' Math Kernel Library} (Intel® MKL) through
#' \href{https://mran.revolutionanalytics.com/open/}{Microsoft
#' R Open} (MRO). It really improves performance
#' of \code{R} and \code{RcppArmadillo} matrix computations,
#' yet not the ones of \code{RcppEigen} (at least not directly).
#'
#' So, \enumerate{
#' \item \code{Eigen} should be prefered if you don't change anything,
#' \item base \code{R} should be prefered if you use MRO,
#' \item \code{Eigen} may be prefered if you manage to link \code{RcppEigen}
#' with the MKL (please \href{mailto:florian.prive.21@gmail.com}{contact me}
#' if you do!).}
#' @seealso \code{\link{tcrossprod}}
#' @return Either \itemize{
#' \item A \code{big.matrix} of type \code{double} if all rows are used
#' in \code{ind.train}.
#' \item Two \code{big.matrix} of type \code{double}. One for
#' \eqn{X.train X.train^T} to get Principal Components
#' and one for \eqn{X.test X.train^T} to project the rest of the data.}
#' @export
#' @example examples/example.BigXYt.R
BigXYt <- function(X,
                   block.size,
                   ind.train = seq(nrow(X)),
                   vec.center = rep(0, ncol(X)),
                   vec.scale = rep(1, ncol(X)),
                   use.Eigen = TRUE,
                   progress = TRUE) {
  check_X(X)

  progress <- progress & interactive()
  n <- length(ind.train)

  if ((n2 <- nrow(X) - n) != 0) {
    bigK2 <- bigmemory::big.matrix(n2, n, type = "double",
                                   init = 0, shared = FALSE)
  }
  bigK <- bigmemory::big.matrix(n, n, type = "double",
                                init = 0, shared = FALSE)

  # function to compute X*X^T
  intervals <- CutBySize(ncol(X), block.size)
  nb.block <- nrow(intervals)

  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = nb.block, style = 3)
  }

  for (j in 1:nb.block) {
    if (progress) utils::setTxtProgressBar(pb, j - 1)
    ind <- seq2(intervals[j, ])
    mean <- vec.center[ind]
    sd <- vec.scale[ind]
    tmp <- scaling(X[ind.train, ind], mean, sd)
    if (use.Eigen) {
      tcrossprodEigen(bigK@address, tmp)
    } else {
      incrSup(bigK@address, tcrossprod(tmp))
    }
    if (n2 != 0) {
      if (use.Eigen) {
        tcrossprodEigen2(bigK2@address,
                         scaling(X[-ind.train, ind], mean, sd),
                         tmp)
      } else {
        incrAll(bigK2@address,
                tcrossprod(scaling(X[-ind.train, ind], mean, sd), tmp))
      }
    }
  }

  # Complete the lower part of the symmetric matrix
  complete(bigK@address)

  if (progress) {
    utils::setTxtProgressBar(pb, nb.block)
    close(pb)
  }

  if (n2 == 0) {
    return(bigK)
  } else {
    return(list(bigK, bigK2))
  }
}
