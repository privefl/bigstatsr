#' Some scaling functions
#'
#' Some scaling functions for a `big.matrix` to be used as
#' the __`fun.scaling`__ parameter of some functions of this package.
#'
#' @inheritParams bigstatsr-package
#' @param center A logical value: whether to return means or 0s.
#' @param scale A logical value: whether to return sds or 1s. __You can't
#' use scale without using center.__
#' @return
#' A new __function__ that returns a named list of two vectors "mean" and "sd"
#' which are as long as the number of columns of `X`.
#' @seealso [scale]
#' @example examples/example-scaling.R
#' @export
big_scale <- function(center = TRUE, scale = TRUE) {
  function(X, ind.train = seq(nrow(X))) {
    m <- ncol(X)
    if (center) {
      tmp <- big_colstats(X, ind.train)
      means <- tmp$sum / length(ind.train)
      sds <- `if`(scale, sqrt(tmp$var), rep(1, m))
    } else {
      means <- rep(0, m)
      sds <- rep(1, m)
    }
    list(mean = means, sd = sds)
  }
}
