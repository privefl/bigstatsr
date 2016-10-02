#' @import bigmemory
#' @useDynLib bigstatsr
#' @importFrom Rcpp sourceCpp
#' @param X A [big.matrix][bigmemory::big.matrix-class].
#' You shouldn't have missing values in your data.
#' @param y Either
#' - a vector of \\{-1, 1\\} in the case of classification (suffix Class),
#' - a vector of more than two unique values
#' in the case of regression (suffix Reg).
#' @param ind.train An optional vector of the row indices that are used,
#' for the training part.
#' If not specified, all data are used.
"_PACKAGE"
