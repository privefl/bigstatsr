################################################################################

DualBigPCA <- function(X, fun.scaling,
                       ind.row,
                       block.size,
                       k,
                       thr.eigval) {

  tmp <- big_tcrossprodSelf(X,
                            fun.scaling = fun.scaling,
                            ind.row = ind.row,
                            block.size = block.size)

  eig <- `if`(is.null(k),
              eigen(tmp$K, symmetric = TRUE),
              RSpectra::eigs_sym(tmp$K, k))
  tmp$K <- NULL

  lastEig <- max(which(eig$values > (thr.eigval * ncol(X))))

  u <- eig$vectors[, 1:lastEig]
  d <- sqrt(eig$values[1:lastEig])
  rm(eig)

  # crossprod with clever scaling -> see vignettes
  v <- (big_cprodMat(X, u, ind.row, block.size = block.size) -
          tcrossprod(tmp$mean, colSums(u))) / tmp$sd
  v <- scaling(v, rep(0, lastEig), d)

  list(d = d, u = u, v = v, means = tmp$mean, sds = tmp$sd)
}

################################################################################

PrimalBigPCA <- function(X, fun.scaling,
                         ind.row,
                         block.size,
                         k,
                         thr.eigval) {

  tmp <- big_crossprodSelf(X,
                           fun.scaling = fun.scaling,
                           ind.row = ind.row,
                           block.size = block.size)

  eig <- `if`(is.null(k),
              eigen(tmp$K, symmetric = TRUE),
              RSpectra::eigs_sym(tmp$K, k))
  tmp$K <- NULL

  lastEig <- max(which(eig$values > (thr.eigval * length(ind.row))))

  v <- eig$vectors[, 1:lastEig]
  d <- sqrt(eig$values[1:lastEig])
  rm(eig)

  # multiplication with clever scaling -> see vignettes
  v2 <- v / tmp$sd
  u <- big_prodMat(X, v2, ind.row = ind.row, block.size = block.size)
  u <- scaling(u, crossprod(tmp$mean, v2), d)

  list(d = d, u = u, v = v, means = tmp$mean, sds = tmp$sd)
}

################################################################################

#' SVD
#'
#' An algorithm for SVD (or PCA) of a `big.matrix` through the eigen
#' decomposition of the covariance between variables (primal)
#' or observations (dual).
#'
#' To get \eqn{X = U \cdot D \cdot V^T},
#' - if the number of observation is small, this function computes
#'   \eqn{K_(2) = X \cdot X^T = U \cdot D^2 \cdot U^T} and then
#'   \eqn{V = X^T \cdot U \cdot D^{-1}},
#' - if the number of variable is small, this function computes
#'   \eqn{K_(1) = X^T \cdot X = V \cdot D^2 \cdot V^T} and then
#'   \eqn{U = X \cdot V \cdot D^{-1}},
#' - if both dimensions are large and you only want a partial SVD,
#'   please use [big_randomSVD] instead.
#'
#' @inheritParams bigstatsr-package
#' @param k Number of singular vectors/values to compute. Default is all.
#'
#' @export
#' @return A named list (an S3 class "big_SVD") of
#' - `d`, the singular values,
#' - `u`, the left singular vectors,
#' - `v`, the right singular vectors,
#' - `means`, the centering vector,
#' - `sds`, the scaling vector.
#'
#' Note that to obtain the Principal Components, you must use
#' [predict][predict.big_SVD] on the result. See examples.
#'
#' @example examples/example-SVD.R
#' @seealso [prcomp][stats::prcomp]
big_SVD <- function(X., fun.scaling,
                    ind.row = rows_along(X.),
                    block.size = 1000,
                    k = NULL,
                    thr.eigval = 1e-4) {
  X <- attach.BM(X.)
  if (ncol(X) > length(ind.row)) {
    printf("(2)")
    res <- DualBigPCA(X, fun.scaling, ind.row, block.size, k, thr.eigval)
  } else {
    printf("(1)")
    res <- PrimalBigPCA(X, fun.scaling, ind.row, block.size, k, thr.eigval)
  }

  structure(res, class = "big_SVD")
}

################################################################################
