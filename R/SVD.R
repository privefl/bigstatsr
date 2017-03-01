################################################################################

DualBigPCA <- function(X, fun.scaling,
                       ind.row,
                       block.size,
                       k,
                       thr.eigval) {

  tmp <- big_tcrossprodSelf(X = X,
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

  tmp <- big_crossprodSelf(X = X,
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
#' @inheritParams bigstatsr-package
#' @param k Number of singular vectors/values to compute. Default is all.
#'
#' @export
#' @return A list of
#' - `d`, the singular values,
#' - `u`, the left singular vectors,
#' - `v`, the right singular vectors,
#' - `means`, the centering vector,
#' - `sds`, the scaling vector.
#'
#' Note that to obtain the Principal Components, you must use
#' `big_predScoresPCA` on the result. See examples.
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
    DualBigPCA(X, fun.scaling, ind.row, block.size, k, thr.eigval)
  } else {
    printf("(1)")
    PrimalBigPCA(X, fun.scaling, ind.row, block.size, k, thr.eigval)
  }
}

################################################################################

#' Scores of PCA
#'
#' Get the scores of PCA associated with an svd decomposition
#' using function `big_SVD`.
#'
#' @inheritParams bigstatsr-package
#' @param obj.svd A list returned by `big_SVD` or `big_randomSVD`.
#'
#' @export
#' @return A matrix of size \eqn{n \times K} where `n` is the number of samples
#' corresponding to indices in `ind.row` and K the number of PCs
#' computed in `obj.svd`. If `X` is not specified, this just returns
#' the scores of the training set of `obj.svd`.
#'
#' @example examples/example-SVD.R
#' @seealso [predict][stats::prcomp] [big_SVD] [big_randomSVD]
big_predScoresPCA <- function(obj.svd, X. = NULL,
                              ind.row = rows_along(X.),
                              ind.col = cols_along(X.),
                              block.size = 1000) {
  if (is.null(X.)) {
    obj.svd$u %*% diag(obj.svd$d)
  } else {
    X <- attach.BM(X.)
    # multiplication with clever scaling -> see vignettes
    v2 <- obj.svd$v / obj.svd$sds
    tmp <- big_prodMat(X, v2, ind.row, ind.col, block.size)
    sweep(tmp, 2, crossprod(obj.svd$means, v2), '-')
  }
}

################################################################################
