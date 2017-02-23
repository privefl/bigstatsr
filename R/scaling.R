#' Some scaling functions
#'
#' Some scaling functions for a `big.matrix` to be used as
#' the __`fun.scaling`__ parameter of some functions of this package.
#'
#' @param center A logical value: whether to return means or 0s.
#' @param scale A logical value: whether to return sds or 1s. __You can't
#' use scale without using center.__
#' @return
#' A new __function__ that returns a data.frame of two vectors
#' "mean" and "sd" which are of the length of __`ind.col`__.
#' @seealso [scale]
#' @example examples/example-scaling.R
#' @export
big_scale <- function(center = TRUE, scale = TRUE) {
  function(X, ind.row = seq(nrow(X)), ind.col = seq(ncol(X))) {
    m <- length(ind.col)
    if (center) {
      tmp <- big_colstats(X, ind.row, ind.col)
      means <- tmp$sum / length(ind.row)
      sds <- `if`(scale, sqrt(tmp$var), rep(1, m))
    } else {
      means <- rep(0, m)
      sds <- rep(1, m)
    }
    data.frame(mean = means, sd = sds)
  }
}
