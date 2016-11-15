################################################################################

DualBigPCA <- function(X, fun.scaling,
                       ind.train = seq(nrow(X)),
                       block.size = 1e3,
                       k = NULL,
                       thr.eigval = 1e-3,
                       use.Eigen = !detect_MRO(),
                       returnU = TRUE,
                       returnV = TRUE) {

  tmp <- big_tcrossprodSelf(X = X,
                            fun.scaling = fun.scaling,
                            ind.train = ind.train,
                            block.size = block.size,
                            use.Eigen = use.Eigen,
                            returnScale = TRUE)

  eig <- `if`(is.null(k),
              eigen(tmp$K[,], symmetric = TRUE),
              RSpectra::eigs_sym(tmp$K[,], k))

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
                         ind.train = seq(nrow(X)),
                         block.size = 1e3,
                         k = NULL,
                         thr.eigval = 1e-3,
                         use.Eigen = TRUE,
                         returnU = TRUE,
                         returnV = TRUE) {

  tmp <- big_crossprodSelf(X = X,
                           fun.scaling = fun.scaling,
                           ind.train = ind.train,
                           block.size = block.size,
                           use.Eigen = use.Eigen,
                           returnScale = TRUE)

  eig <- `if`(is.null(k),
              eigen(tmp$K[,], symmetric = TRUE),
              RSpectra::eigs_sym(tmp$K[,], k))

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
#' @return
#'
#' @example examples/example-SVD.R
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
