################################################################################

DualBigPCA <- function(X, fun.scaling,
                       ind.row,
                       ind.col,
                       block.size,
                       k) {

  K <- big_tcrossprodSelf(X, fun.scaling = fun.scaling,
                          ind.row = ind.row,
                          ind.col = ind.col,
                          block.size = block.size)
  means <- attr(K, "center")
  sds <- attr(K, "scale")

  # compute eigen values/vectors
  eig <- RSpectra::eigs_sym(K, k)
  u <- eig$vectors
  d <- sqrt(eig$values)
  # no longer need K (and eig)
  rm(K, eig)

  # crossprod with clever scaling -> see vignettes
  v <- (big_cprodMat(X, u, ind.row, ind.col, block.size = block.size) -
          tcrossprod(means, colSums(u))) / sds
  v <- sweep(v, 2, d, "/")

  list(d = d, u = u, v = v, center = means, scale = sds)
}

################################################################################

PrimalBigPCA <- function(X, fun.scaling,
                         ind.row,
                         ind.col,
                         block.size,
                         k) {

  K <- big_crossprodSelf(X, fun.scaling = fun.scaling,
                         ind.row = ind.row,
                         ind.col = ind.col,
                         block.size = block.size)
  means <- attr(K, "center")
  sds <- attr(K, "scale")

  # compute eigen values/vectors
  eig <- RSpectra::eigs_sym(K, k)
  v <- eig$vectors
  d <- sqrt(eig$values)
  # no longer need K (and eig)
  rm(K, eig)

  # multiplication with clever scaling -> see vignettes
  v2 <- v / sds
  u <- big_prodMat(X, v2, ind.row, ind.col, block.size = block.size)
  u <- scaling(u, crossprod(means, v2), d)

  list(d = d, u = u, v = v, center = means, scale = sds)
}

################################################################################

#' Partial SVD
#'
#' An algorithm for partial SVD (or PCA) of a Filebacked Big Matrix through the
#' eigen decomposition of the covariance between variables (primal)
#' or observations (dual). **Use this algorithm only if there is one dimension
#' that is much smaller than the other. Otherwise use [big_randomSVD].**
#'
#' To get \eqn{X = U \cdot D \cdot V^T},
#' - if the number of observations is small, this function computes
#'   \eqn{K_(2) = X \cdot X^T \approx U \cdot D^2 \cdot U^T} and then
#'   \eqn{V = X^T \cdot U \cdot D^{-1}},
#' - if the number of variable is small, this function computes
#'   \eqn{K_(1) = X^T \cdot X \approx V \cdot D^2 \cdot V^T} and then
#'   \eqn{U = X \cdot V \cdot D^{-1}},
#' - if both dimensions are large, use [big_randomSVD] instead.
#'
#' @inheritParams bigstatsr-package
#' @param k Number of singular vectors/values to compute. Default is `10`.
#'
#' @export
#' @return A named list (an S3 class "big_SVD") of
#' - `d`, the singular values,
#' - `u`, the left singular vectors,
#' - `v`, the right singular vectors,
#' - `center`, the centering vector,
#' - `scale`, the scaling vector.
#'
#' Note that to obtain the Principal Components, you must use
#' [predict][predict.big_SVD] on the result. See examples.
#'
#' @example examples/example-SVD.R
#' @seealso [prcomp][stats::prcomp]
#'
big_SVD <- function(X, fun.scaling,
                    ind.row = rows_along(X),
                    ind.col = cols_along(X),
                    k = 10,
                    block.size = block_size(nrow(X))) {

  check_args()

  if (ncol(X) > length(ind.row)) {
    printf("(2)")
    res <- DualBigPCA(X, fun.scaling, ind.row, ind.col, block.size, k)
  } else {
    printf("(1)")
    res <- PrimalBigPCA(X, fun.scaling, ind.row, ind.col, block.size, k)
  }

  structure(res, class = "big_SVD")
}

################################################################################
