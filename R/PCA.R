################################################################################

#' @name BigXYt
#' @description Compute linear kernel matrices for a big.matrix
#' after applying a particular scaling to it.
#' @title Linear kernel matrices for a big.matrix.
#' @inheritParams bigstatsr-package
#' @param block.size Maximum number of columns read at once.
#' @param vec.center Vector that will be subtracted to the matrix,
#' columnwise. Typically, the mean of each column.
#' See \code{\link{colmeans}}.
#' @param vec.scale Vector that will be divided to the matrix
#' (after the substraction), columnwise.
#' Typically, the sd of each column? See \code{\link{colsds}}.
#' @param progress Use a progress bar?
#' @param use.Eigen Use the \code{Eigen} library to compute \eqn{X X^T},
#' the default.
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
                   vec.center = rep(0, length(ind.train)),
                   vec.scale = rep(1, length(ind.train)),
                   use.Eigen = TRUE,
                   progress = TRUE) {
  if (class(X) != "big.matrix") stop("X must be a big.matrix")

  progress <- progress & interactive()
  n <- length(ind.train)

  if ((n2 <- nrow(X) - n) != 0) {
    bigK2 <- bigmemory::big.matrix(n2, n, type = "double",
                                   init = 0, shared = F)
  }
  bigK <- bigmemory::big.matrix(n, n, type = "double",
                                init = 0, shared = F)

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

################################################################################

#' #' @name PCA.bigSNP
#' #' @title Principal Components of a "bigSNP".
#' #' @description Get k or all Principal Components (PCs) of a \code{bigSNP}
#' #' @inheritParams BigXYt
#' #' @param k Number of PCs to compute. Default is all.
#' #' @param thr.eigval Threshold to remove "unsignificant" PCs.
#' #' Default is \code{1e-3}.
#' #' @export
#' #' @return A \code{matrix} of PCs.
#' #' @details See \code{\link{BigXYt}}.
#' #'
#' #' Note that for the Eigen decomposition, only \code{R} is
#' #' used because is faster (see \href{http://goo.gl/UYJcCw}{stackoverflow}).
#' #' If you want a large number of eigenvectors/values, you should
#' #' really considerer using Microsoft R Open for speed.
#' #' @example examples/example.PCA.bigSNP.R
#' #' @seealso \code{\link{bigSNP}} \code{\link{prcomp}}
#' PCA.bigSNP <- function(x,
#'                        block.size,
#'                        k = NULL,
#'                        ind.train = NULL,
#'                        thr.eigval = 1e-3,
#'                        use.Eigen = TRUE) {
#'   if (class(X) != "big.matrix") stop("X must be a big.matrix")
#'
#'   res <- BigXYt(x, block.size, ind.train, use.Eigen)
#'   n.all <- nrow(x$genotypes)
#'   if (only1 <- is.null(ind.train)) {
#'     bigK <- res
#'     ind.train <- 1:n.all
#'   } else {
#'     bigK  <- res[[1]]
#'     bigK2 <- res[[2]]
#'   }
#'   rm(res)
#'
#'   n <- nrow(bigK)
#'   means <- bigcolsumsDouble(bigK@address) / n
#'   symCenter(bigK@address, means, mean(means))
#'   if (!only1) colCenter(bigK2@address, means)
#'
#'   if (is.null(k)) {
#'     eig <- eigen(bigK[,], symmetric = TRUE)
#'   } else {
#'     eig <- RSpectra::eigs_sym(bigK[,], k)
#'   }
#'
#'   alphas <- scaling(eig$vectors,
#'                     rep(0, length(eig$values)),
#'                     sqrt(eig$values))
#'   m <- ncol(x$genotypes)
#'   lastEig <- max(which(eig$values > (thr.eigval * m)))
#'   rm(eig)
#'   alphas <- alphas[, 1:lastEig]
#'
#'   rotated <- matrix(0, n.all, lastEig)
#'   rotated[ind.train, ] <- bigK[,] %*% alphas
#'   if (!only1) rotated[-ind.train, ] <- bigK2[,] %*% alphas
#'
#'   return(rotated)
#' }
