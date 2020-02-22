################################################################################

#' Crossprod
#'
#' Compute \eqn{X.row^T X.row} for a Filebacked Big Matrix `X`
#' after applying a particular scaling to it.
#'
#' @inheritParams bigstatsr-package
#' @inheritSection bigstatsr-package Matrix parallelization
#'
#' @return A temporary [FBM][FBM-class], with the following two attributes:
#' - a numeric vector `center` of column scaling,
#' - a numeric vector `scale` of column scaling.
#' @export
#' @seealso [crossprod]
#'
#' @example examples/example-crossprodSelf.R
#'
big_crossprodSelf <- function(
  X,
  fun.scaling = big_scale(center = FALSE, scale = FALSE),
  ind.row = rows_along(X),
  ind.col = cols_along(X),
  block.size = block_size(nrow(X))
) {

  check_args()

  m <- length(ind.col)
  K <- FBM(m, m)

  mu    <- numeric(m)
  delta <- numeric(m)
  sums  <- numeric(m)

  intervals <- CutBySize(m, block.size)

  for (j in rows_along(intervals)) {

    ind1 <- seq2(intervals[j, ])
    tmp1 <- X[ind.row, ind.col[ind1]]

    ms <- fun.scaling(X, ind.row = ind.row, ind.col = ind.col[ind1])
    mu[ind1]    <- ms$center
    delta[ind1] <- ms$scale
    sums[ind1]  <- colSums(tmp1)

    K[ind1, ind1] <- crossprod(tmp1)

    next_lower <- intervals[j, "upper"] + 1L
    if (next_lower <= m) {
      ind2 <- next_lower:m
      K.part <- big_cprodMat(X, tmp1, ind.row, ind.col[ind2],
                             block.size = block.size) # TODO: add ncores
      K[ind2, ind1] <- K.part
      K[ind1, ind2] <- t(K.part)
    }
  }

  # "Scale" the cross-product (see https://goo.gl/HK2Bqb)
  scaleK(K, sums = sums, mu = mu, delta = delta, nrow = length(ind.row))
  structure(K, center = mu, scale = delta)
}

################################################################################

#' Correlation
#'
#' Compute the correlation matrix of a Filebacked Big Matrix.
#'
#' @inherit big_crossprodSelf params return
#' @inheritSection bigstatsr-package Matrix parallelization
#'
#' @export
#' @seealso [cor] [big_crossprodSelf]
#'
#' @example examples/example-corr.R
#'
big_cor <- function(X,
                    ind.row = rows_along(X),
                    ind.col = cols_along(X),
                    block.size = block_size(nrow(X))) {

  cor.scaling <- function(X, ind.row, ind.col) {
    ms <- big_scale(center = TRUE, scale = TRUE)(X, ind.row, ind.col)
    ms$scale <- ms$scale * sqrt(length(ind.row) - 1)
    ms
  }

  big_crossprodSelf(X, fun.scaling = cor.scaling,
                    ind.row = ind.row,
                    ind.col = ind.col,
                    block.size = block.size)
}

################################################################################

#' @export
#' @param x A 'double' FBM.
#' @param y Missing.
#' @rdname big_crossprodSelf
setMethod("crossprod", signature(x = "FBM", y = "missing"),
          function(x, y) crossprod_FBM(x))

################################################################################
