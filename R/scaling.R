#' Some scaling functions.
#'
#' Some scaling functions for a "big.matrix" to be used as
#' the `fun.scaling` parameter of some functions of this package.
#'
#' @inheritParams bigstatsr-package
#' @return
#' A named list of two vectors "mean" and "sd" which are as long as
#' the number of columns of `X`. Equivalence with [scale]:
#' - `big_noscale`: `center = FALSE` & `scale = FALSE`,
#' - `big_center`:  `center = TRUE`  & `scale = FALSE`,
#' - `big_scale`:   `center = TRUE`  & `scale = TRUE`.
#' @examples
#' print(big_center)
#' print(big_scale)
#' print(big_noscale)
#' @name big_funScaling


################################################################################

#' @export
#' @rdname big_funScaling
big_center <- function(X, ind.train = seq(nrow(X))) {
  means <- colmeans(X, ind.train)
  list(mean = means, sd = rep(1, ncol(X)))
}

################################################################################

#' @export
#' @rdname big_funScaling
big_scale <- function(X, ind.train = seq(nrow(X))) {
  colmeans_sds(X, ind.train)
}

################################################################################

#' @export
#' @rdname big_funScaling
big_noscale <- function(X, ind.train = seq(nrow(X))) {
  m <- ncol(X)
  list(mean = rep(0, m), sd = rep(1, m))
}

################################################################################
