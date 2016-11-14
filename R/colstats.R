#' Standard univariate statistics
#'
#' Standard __univariate statistics__ for columns of a big.matrix.
#' For now, the `sum` and `var` are implemented
#' (the `mean` and `sd` can easily be deduced, see examples).
#'
#' @inheritParams bigstatsr-package
#'
#' @return List of two numeric vectors `sum` and `var` with the
#' corresponding column statistics.
#' @export
#'
#' @seealso [colSums] [apply]
#' @example examples/example-colstats.R
big_colstats <- function(X, ind.train = seq_len(nrow(X))) {
  check_X(X)
  bigcolvars(X@address, ind.train)
}
