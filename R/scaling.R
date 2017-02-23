################################################################################

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
#'
#' @seealso [scale]
#'
#' @export
#'
#' @examples
#' X.desc <- big_attachExtdata()
#'
#' # No scaling
#' big_noscale <- big_scale(center = FALSE, scale = FALSE)
#' class(big_noscale) # big_scale returns a new function
#' str(big_noscale(X.desc))
#' big_noscale2 <- big_scale(center = FALSE)
#' str(big_noscale2(X.desc)) # you can't scale without centering
#'
#' # Centering
#' big_center <- big_scale(scale = FALSE)
#' str(big_center(X.desc))
#' # + scaling
#' str(big_scale()(X.desc))
big_scale <- function(center = TRUE, scale = TRUE) {
  function(X., ind.row = rows_along(X.), ind.col = cols_along(X.)) {
    m <- length(ind.col)
    if (center) {
      tmp <- big_colstats(X., ind.row, ind.col)
      means <- tmp$sum / length(ind.row)
      sds <- `if`(scale, sqrt(tmp$var), rep(1, m))
    } else {
      means <- rep(0, m)
      sds <- rep(1, m)
    }
    data.frame(mean = means, sd = sds)
  }
}

################################################################################
