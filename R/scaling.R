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

#' Y-aware scaling
#'
#' @inheritParams bigstatsr-package
#' @return
#' A new __function__ that returns a named list of
#'  two vectors __`mean`__ and __`sd`__
#' which are as long as the number of columns of __`X`__.
#'
#' @details "y-aware" scaling center columns, then multiply them
#' by betas of univariate linear regression.
#' So, __`mean`__ are column means and __`sd`__ are the inverse of slopes.
#' See \href{https://goo.gl/8G8WMa}{this blog post} for details.
#'
#' @export
#'
#' @examples
#' # Simulating some data
#' X <- big.matrix(41, 17)
#' X[] <- rnorm(length(X))
#' y <- rnorm(nrow(X), X[, 9], abs(X[, 9]))
#'
#' X.svd <- bigstatsr::big_SVD(X, fun.scaling = big_scaleYaware(y))
#' plot(X.svd$v[, 1], type = "h")
big_scaleYaware <- function(y) {
  function(X, ind.train = seq(nrow(X))) {
    means <- big_colstats(X, ind.train)$sum / length(ind.train)
    betas <- big_univRegLin(X, y, ind.train)$estim
    list(mean = means, sd = 1 / betas)
  }
}
