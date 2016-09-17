################################################################################

#' @name DualBigPCA
#' @title Principal Components of a "big.matrix".
#' @description Get k or all Principal Components (PCs) of a \code{big.matrix}
#' which has more columns than rows.
#' @inheritParams BigXYt
#' @param k Number of PCs to compute. Default is all.
#' @param thr.eigval Threshold to remove "unsignificant" PCs.
#' Default is \code{1e-3}.
#' @export
#' @return A \code{matrix} of PCs.
#' @details See \code{\link{BigXYt}}.
#'
#' Note that for the Eigen decomposition, only \code{R} is
#' used because is faster (see \href{http://goo.gl/UYJcCw}{stackoverflow}).
#' If you want a large number of eigenvectors/values, you should
#' really considerer using Microsoft R Open for speed.
#' @example examples/example.DualBigPCA.R
#' @seealso \code{\link{prcomp}}
DualBigPCA <- function(X,
                       block.size,
                       k = NULL,
                       ind.train = seq(nrow(X)),
                       vec.center = rep(0, length(ind.train)),
                       vec.scale = rep(1, length(ind.train)),
                       thr.eigval = 1e-3,
                       use.Eigen = TRUE,
                       progress = TRUE) {
  if (class(X) != "big.matrix") stop("X must be a big.matrix")

  res <- BigXYt(X = X,
                block.size = block.size,
                ind.train = ind.train,
                vec.center = vec.center,
                vec.scale = vec.scale,
                use.Eigen = use.Eigen,
                progress = progress)

  n <- length(ind.train)
  if (((n2 <- nrow(X) - n) == 0)) {
    bigK <- res
  } else {
    bigK  <- res[[1]]
    bigK2 <- res[[2]]
  }
  rm(res)

  #means <- bigcolsumsDouble(bigK@address) / n
  #symCenter(bigK@address, means, mean(means))
  #if (!only1) colCenter(bigK2@address, means)

  if (is.null(k)) {
    eig <- eigen(bigK[,], symmetric = TRUE)
  } else {
    eig <- RSpectra::eigs_sym(bigK[,], k)
  }

  lastEig <- max(which(eig$values > (thr.eigval * ncol(X))))
  alphas <- scaling(eig$vectors[, 1:lastEig],
                    rep(0, lastEig),
                    sqrt(eig$values[1:lastEig]))

  rm(eig)

  rotated <- matrix(0, nrow(X), lastEig)
  rotated[ind.train, ] <- bigK[,] %*% alphas
  if (n2 != 0) rotated[-ind.train, ] <- bigK2[,] %*% alphas

  return(rotated)
}

################################################################################
