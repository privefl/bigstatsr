################################################################################

DualBigPCA <- function(X, fun.scaling,
                       ind.row,
                       block.size,
                       k,
                       thr.eigval,
                       use.Eigen) {

  tmp <- big_tcrossprodSelf(X = X,
                            fun.scaling = fun.scaling,
                            ind.row = ind.row,
                            block.size = block.size,
                            use.Eigen = use.Eigen)

  eig <- `if`(is.null(k),
              eigen(tmp$K, symmetric = TRUE),
              RSpectra::eigs_sym(tmp$K, k))
  tmp$K <- NULL

  lastEig <- max(which(eig$values > (thr.eigval * ncol(X))))

  u <- eig$vectors[, 1:lastEig]
  d <- sqrt(eig$values[1:lastEig])
  rm(eig)

  v <- crossprodScaled(X, u, ind.row, block.size, tmp$mean, tmp$sd, use.Eigen)
  v <- scaling(v, rep(0, lastEig), d)

  list(d = d, u = u, v = v, means = tmp$mean, sds = tmp$sd)
}

################################################################################

PrimalBigPCA <- function(X, fun.scaling,
                         ind.row,
                         block.size,
                         k,
                         thr.eigval,
                         use.Eigen) {

  tmp <- big_crossprodSelf(X = X,
                           fun.scaling = fun.scaling,
                           ind.row = ind.row,
                           block.size = block.size,
                           use.Eigen = use.Eigen)

  eig <- `if`(is.null(k),
              eigen(tmp$K, symmetric = TRUE),
              RSpectra::eigs_sym(tmp$K, k))
  tmp$K <- NULL

  lastEig <- max(which(eig$values > (thr.eigval * length(ind.row))))

  v <- eig$vectors[, 1:lastEig]
  d <- sqrt(eig$values[1:lastEig])
  rm(eig)

  u <- multScaled(X, v, ind.row, block.size, tmp$mean, tmp$sd, use.Eigen)
  u <- scaling(u, rep(0, lastEig), d)

  list(d = d, u = u, v = v, means = tmp$mean, sds = tmp$sd)
}

################################################################################

#' SVD
#'
#' An algorithm for SVD (or PCA) of a `big.matrix` through the eigen
#' decomposition of the covariance between variables (primal)
#' or observations (dual).
#'
#' @inherit bigstatsr-package params details
#' @param k Number of singular vectors/values to compute. Default is all.
#' @param thr.eigval Threshold to remove "unsignificant" PCs.
#' Default is \code{1e-4}.
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
#' @example examples/example-newScale.R
#' @seealso [prcomp][stats::prcomp]
big_SVD <- function(X, fun.scaling,
                    ind.row = seq(nrow(X)),
                    block.size = 1e3,
                    k = NULL,
                    thr.eigval = 1e-4,
                    use.Eigen = !detect_MRO()) {
  if (ncol(X) > length(ind.row)) {
    printf("(2)")
    DualBigPCA(X, fun.scaling, ind.row, block.size, k, thr.eigval, use.Eigen)
  } else {
    printf("(1)")
    PrimalBigPCA(X, fun.scaling, ind.row, block.size, k, thr.eigval, use.Eigen)
  }
}

################################################################################

#' Scores of PCA
#'
#' Get the scores of PCA associated with an svd decomposition
#' using function `big_SVD`.
#'
#' @inherit bigstatsr-package params
#' @param obj.svd A list returned by `big_SVD` or `big_randomSVD`.
#'
#' @export
#' @return A matrix of size \eqn{n \times K} where `n` is the number of samples
#' corresponding to indices in `ind.row` and K the number of PCs
#' computed in `obj.svd`. If `X` is not specified, this just returns
#' the scores of the training set of `obj.svd`.
#'
#' @example examples/example-SVD.R
#' @seealso [predict][stats::predict.prcomp] [big_SVD]
big_predScoresPCA <- function(obj.svd, X. = NULL,
                              ind.row = rows_along(X.),
                              ind.col = cols_along(X.),
                              block.size = 1000,
                              use.Eigen = !detect_MRO()) {
  if (is.null(X.)) {
    obj.svd$u %*% diag(obj.svd$d)
  } else {
    stopifnot(all(ind.row > 0))
    multScaled2(attach.BM(X.), mat = obj.svd$v,
                ind.row, ind.col, block.size,
                vec.center = obj.svd$means,
                vec.scale = obj.svd$sds,
                use.Eigen)
  }
}

################################################################################
