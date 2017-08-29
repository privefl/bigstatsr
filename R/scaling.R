################################################################################

#' Some scaling functions
#'
#' Some scaling functions for a Filebacked Big Matrix to be used as
#' the **`fun.scaling`** parameter of some functions of this package.
#'
#' One could think about less common scalings, such as for example the
#' "y-aware" scaling which uses the inverse of betas of column-wise linear
#' regression as scaling. See [this post](https://goo.gl/8G8WMa) for details.
#' It would be easy to implement it using `big_colstats` to get column means
#' and `big_univLinReg` to get betas (and then inverse them).
#'
#' @param center A logical value: whether to return means or 0s.
#' @param scale A logical value: whether to return sds or 1s. **You can't
#' use scale without using center.**
#'
#' @return
#' A new **function** that returns a data.frame of two vectors
#' "center" and "scale" which are of the length of `ind.col`.
#'
#' @seealso [scale]
#'
#' @export
#'
#' @examples
#' X <- big_attachExtdata()
#'
#' # No scaling
#' big_noscale <- big_scale(center = FALSE, scale = FALSE)
#' class(big_noscale) # big_scale returns a new function
#' str(big_noscale(X))
#' big_noscale2 <- big_scale(center = FALSE)
#' str(big_noscale2(X)) # you can't scale without centering
#'
#' # Centering
#' big_center <- big_scale(scale = FALSE)
#' str(big_center(X))
#' # + scaling
#' str(big_scale()(X))
big_scale <- function(center = TRUE, scale = TRUE) {

  function(X, ind.row = rows_along(X), ind.col = cols_along(X)) {

    check_args()

    m <- length(ind.col)
    if (center) {
      tmp <- big_colstats(X, ind.row, ind.col)
      means <- tmp$sum / length(ind.row)
      sds <- `if`(scale, sqrt(tmp$var), rep(1, m))
    } else {
      means <- rep(0, m)
      sds <- rep(1, m)
    }

    data.frame(center = means, scale = sds)
  }
}

################################################################################
