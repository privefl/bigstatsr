#' @import bigmemory
#' @useDynLib bigstatsr
#' @importFrom Rcpp sourceCpp
#' @param X A [big.matrix][bigmemory::big.matrix-class].
#' You shouldn't have missing values in your data.
#' @param y Vector of responses.
#' @param ind.train An optional vector of the row indices that are used,
#' for the training part. If not specified, all data are used.
#' @param ncores Number or cores used. Default doesn't use parallelism.
"_PACKAGE"
