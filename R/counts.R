################################################################################

#' Counts
#'
#' Counts by columns (or rows) the number of each unique element of a
#' `FBM.code256`.
#'
#' @inheritParams bigstatsr-package
#' @param byrow Count by rows rather than columns?
#'   Default is `FALSE` (columns).
#'
#' @example examples/example-counts.R
#'
#' @return A matrix of counts of K x m (or n) elements, where
#' - K is the number of unique elements of the `BM.code`,
#' - n is its number of rows,
#' - m is its number of columns.
#'
#' **Beware that K is up to 256. So, if you apply this on a Filebacked Big
#' Matrix of one million columns, you will create a matrix of nearly 1GB!**.
#' @export
big_counts <- function(X.code,
                       ind.row = rows_along(X.code),
                       ind.col = cols_along(X.code),
                       byrow = FALSE) {

  check_args(X.code = "assert_class(X.code, 'FBM.code256')")

  code <- X.code$code256
  code.uniq <- unique(code)
  ind.code <- match(code, code.uniq)

  if (byrow) {
    res <- mycount1(X.code, ind.row, ind.col, ind.code)
    assert_all(colSums(res), length(ind.col))
  } else {
    res <- mycount2(X.code, ind.row, ind.col, ind.code)
    assert_all(colSums(res), length(ind.row))
  }
  rownames(res) <- code.uniq

  res
}

################################################################################
