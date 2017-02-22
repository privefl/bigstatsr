################################################################################

#' Standard univariate statistics
#'
#' Standard __univariate statistics__ for columns of a big.matrix.
#' For now, the `sum` and `var` are implemented
#' (the `mean` and `sd` can easily be deduced, see examples).
#'
#' @inheritParams bigstatsr-package
#'
#' @return Data.frame of two numeric vectors `sum` and `var` with the
#' corresponding column statistics.
#' @export
#'
#' @seealso [colSums] [apply]
#' @example examples/example-colstats.R
big_colstats <- function(X.desc,
                         ind.row = rows_along(X.desc),
                         ind.col = cols_along(X.desc)) {
  address <- big_address(X.desc)
  data.frame(bigcolvars(address, ind.row, ind.col))
}

################################################################################
