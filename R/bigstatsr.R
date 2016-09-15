#' @import bigmemory
#' @useDynLib bigstatsr
#' @importFrom Rcpp sourceCpp
#' @title Statistical Tools for bigmemory Matrices.
#' @description Easy-to-use, efficient, flexible and scalable
#' statistical tools. Use bigmemory matrices.
#' @name bigstatsr-package
#' @param X A big.matrix.
#' @param y Either \itemize{
#' \item a vector of \{-1, 1\}
#' in the case of classification (suffix Class),
#' \item a vector of more than two unique values
#' in the case of regression (suffix Reg).
#' }
#' @param ind.train An optional vector of the row indices that are used,
#' for the training part.
#' If not specified, all data are used.
#' @aliases bigstatsr-package bigstatsr
#' @keywords package
NULL
