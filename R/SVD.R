################################################################################

DualBigPCA <- function(X, fun.scaling,
                       ind.train,
                       block.size,
                       k,
                       thr.eigval,
                       use.Eigen,
                       returnU,
                       returnV) {

  tmp <- big_tcrossprodSelf(X = X,
                            fun.scaling = fun.scaling,
                            ind.train = ind.train,
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

  if (returnV) {
    v <- crossprodScaled(X, u, ind.train, block.size, tmp$mean, tmp$sd, use.Eigen)
    v <- scaling(v, rep(0, lastEig), d)
  } else {
    v <- NULL
  }

  list(d = d, u = `if`(returnU, u, NULL), v = v, means = tmp$mean, sds = tmp$sd)
}

################################################################################

PrimalBigPCA <- function(X, fun.scaling,
                         ind.train,
                         block.size,
                         k,
                         thr.eigval,
                         use.Eigen,
                         returnU,
                         returnV) {

  tmp <- big_crossprodSelf(X = X,
                           fun.scaling = fun.scaling,
                           ind.train = ind.train,
                           block.size = block.size,
                           use.Eigen = use.Eigen)

  eig <- `if`(is.null(k),
              eigen(tmp$K, symmetric = TRUE),
              RSpectra::eigs_sym(tmp$K, k))
  tmp$K <- NULL

  lastEig <- max(which(eig$values > (thr.eigval * length(ind.train))))

  v <- eig$vectors[, 1:lastEig]
  d <- sqrt(eig$values[1:lastEig])
  rm(eig)

  if (returnU) {
    u <- multScaled(X, v, ind.train, block.size, tmp$mean, tmp$sd, use.Eigen)
    u <- scaling(u, rep(0, lastEig), d)
  } else {
    u <- NULL
  }

  list(d = d, u = u, v = `if`(returnV, v, NULL), means = tmp$mean, sds = tmp$sd)
}

################################################################################

#' SVD
#'
#' An algorithm for SVD (or PCA) of a `big.matrix` through the eigen
#' decomposition of the covariance between variables (primal)
#' or observations (dual).
#'
#' @inherit bigstatsr-package params details
#' @param k Number of PCs to compute. Default is all.
#' @param thr.eigval Threshold to remove "unsignificant" PCs.
#' Default is \code{1e-3}.
#' @param returnU Logical whether to return U or not. Default is `TRUE`.
#' @param returnV Logical whether to return V or not. Default is `TRUE`.
#'
#' @export
#' @return A list of
#' - `d`, the singular values,
#' - `u`, the left singular vectors if `returnU` is `TRUE`,
#' - `v`, the right singular vectors if `returnV` is `TRUE`,
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
                    ind.train = seq(nrow(X)),
                    block.size = 1e3,
                    k = NULL,
                    thr.eigval = 1e-3,
                    use.Eigen = !detect_MRO(),
                    returnU = TRUE,
                    returnV = TRUE) {
  check_X(X)

  if (ncol(X) > length(ind.train)) {
    printf("(2)")
    DualBigPCA(X, fun.scaling, ind.train, block.size, k,
               thr.eigval, use.Eigen, returnU, returnV)
  } else {
    printf("(1)")
    PrimalBigPCA(X, fun.scaling, ind.train, block.size, k,
                 thr.eigval, use.Eigen, returnU, returnV)
  }
}

################################################################################

#' Scores of PCA
#'
#' Get the scores of PCA associated with an svd decomposition
#' using function `big_SVD`.
#'
#' @inherit bigstatsr-package params
#' @param obj.svd A list returned by `big_SVD`.
#' @param ind.test Vector of indices of samples to be projected.
#' Don't use negative indices here.
#'
#' @export
#' @return A matrix of size `n * K` where n is the number of samples
#' corresponding to indices of `ind.test` and K the number of PCs
#' computed in `obj.svd`. If `X` is not specified, this just returns
#' the scores of the training set of `obj.svd`.
#'
#' @example examples/example-SVD.R
#' @seealso [predict][stats::predict.prcomp] [big_SVD]
big_predScoresPCA <- function(obj.svd, X = NULL,
                              ind.test = seq(nrow(X)),
                              block.size = 1000,
                              use.Eigen = !detect_MRO()) {
  if (is.null(X)) {
    obj.svd$u %*% diag(obj.svd$d)
  } else {
    stopifnot(all(ind.test > 0))
    multScaled(X, mat = obj.svd$v,
               ind.test, block.size,
               vec.center = obj.svd$means,
               vec.scale = obj.svd$sds,
               use.Eigen)
  }
}

################################################################################
