################################################################################

#' Partial SVD
#'
#' A randomized algorithm for SVD (or PCA) of a "big.matrix".
#'
#' @inherit bigstatsr-package params details
#' @param K Number of PCs to compute. This algorithm shouldn't
#' be used to compute a lot of PCs. Default is `10`.
#' @param I The number of iterations of the algorithm. Default is `20`.
#' @param backingpath If `X` is filebacked and parallelism is used,
#' the path where are stored the files that are backing `X`.
#'
#' @import foreach
#'
#' @return
#' @export
#'
#' @example examples/example-randomSVD.R
#' @seealso [big_scale] [prcomp][stats::prcomp] [svd]
#' @references The "blanczos" algorithm in
#' Rokhlin, V., Szlam, A., & Tygert, M. (2010).
#' A Randomized Algorithm for Principal Component Analysis.
#' SIAM Journal on Matrix Analysis and Applications, 31(3), 1100–1124.
#' doi:10.1137/080736417
big_randomSVD <- function(X, fun.scaling,
                          ind.train = seq(nrow(X)),
                          block.size = 1e3,
                          K = 10, I = 20,
                          use.Eigen = !detect_MRO(),
                          ncores = 1,
                          backingpath = NULL) {
  check_X(X)

  if (length(ind.train) < ncol(X)) {
    printf("(1)")
    ParallelRandomSVD1(X, fun.scaling, ind.train, block.size,
                       K, I, use.Eigen, backingpath, ncores)
  } else {
    printf("(2)")
    ParallelRandomSVD2(X, fun.scaling, ind.train, block.size,
                       K, I, use.Eigen, backingpath, ncores)
  }
}

################################################################################
