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
big_colstats <- function(X.,
                         ind.row = rows_along(X.),
                         ind.col = cols_along(X.)) {
  X <- attach.BM(X.)
  data.frame(bigcolvars(X, ind.row, ind.col))
}

################################################################################

#' Standardize a double "big.matrix"
#'
#' @inheritParams bigstatsr-package
#' @param thr.sd Threshold on standard deviation under which to ignore a column.
#'
#' @return A vector of column indices that have a low standard deviation.
#' @export
#'
#' @examples
#' tmp <- tmpFBM(descriptor = FALSE)(10, 5, type = "double")
#' tmp[] <- rnorm(length(tmp))
#' apply(tmp[,], 2, function(x) c(mean(x), sd(x)))
#'
#' big_standardize(tmp)
#' apply(tmp[,], 2, function(x) c(mean(x), sd(x)))
big_standardize <- function(X., thr.sd = 1e-4) {
  assert_type(X., "double")
  standardize(attach.BM(X.)@address, thr.sd)
}

################################################################################
