################################################################################

#' Counts
#'
#' Counts by columns (or rows) the number of each unique element of a `BM.code`.
#'
#' @inheritParams bigstatsr-package
#' @param byrow Count by rows rather than columns? Default is `FALSE`.
#'
#' @examples
#' set.seed(11)
#'
#' a <- matrix(as.raw(0), 14, 11)
#' a[] <- sample(as.raw(0:3), size = length(a), replace = TRUE)
#' X <- as.big.matrix(a)
#' X[,]
#'
#' code <- rep(NA_real_, 256)
#' code[1:4] <- c(2, 5, 9, NA)
#' X2 <- as.BM.code(X, code)
#' X2[,]
#'
#' # by columns
#' big_counts(X2)
#' apply(X2[,], 2, table, exclude = NULL)
#'
#' # by rows
#' big_counts(X2, byrow = TRUE)
#' apply(X2[,], 1, table, exclude = NULL)
#'
#' @return A matrix of counts of K x m (or n) elements, where
#' - K is the number of unique elements of the `BM.code`,
#' - n is its number of rows,
#' - m is its number of columns.
#'
#' __Beware that K is up to 256. So, if you apply this on a `big.matrix` of
#' one million columns, you will create a matrix of nearly 1Gb!__.
#' @export
big_counts <- function(X.code,
                       ind.row = rows_along(X.code),
                       ind.col = cols_along(X.code),
                       byrow = FALSE) {

  check_args(X.code = "assert_classOrDesc(X.code, 'BM.code')")

  code <- X.code@code
  code.uniq <- unique(code)
  ind.code <- match(code, code.uniq)

  X <- attach.BM(X.code)
  if (byrow) {
    res <- mycount1(X@address, ind.row, ind.col, ind.code)
    assert_all(colSums(res), length(ind.col))
  } else {
    res <- mycount2(X@address, ind.row, ind.col, ind.code)
    assert_all(colSums(res), length(ind.row))
  }
  rownames(res) <- code.uniq

  res
}

################################################################################
