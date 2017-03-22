################################################################################

#' @import bigmemory
#' @useDynLib bigstatsr
#' @importFrom Rcpp sourceCpp
#'
#' @param X A [big.matrix][big.matrix-class].
#' @param X.desc A [big.matrix.descriptor][big.matrix.descriptor-class].
#' @param X. Either a [big.matrix][big.matrix-class] or
#' a [big.matrix.descriptor][big.matrix.descriptor-class].
#' @param X.code Either a [BM.code][BM.code-class] or
#' a [BM.code.descriptor][BM.code.descriptor-class].
#'
#' @param y.train Vector of responses, corresponding to `ind.train`.
#' @param y01.train Vector of responses, corresponding to `ind.train`.
#' __Must be only 0s and 1s.__
#'
#' @param ind.train An optional vector of the row indices that are used,
#' for the training part. If not specified, all rows are used.
#' __Don't use negative indices.__
#' @param ind.row An optional vector of the row indices that are used.
#' If not specified, all rows are used. __Don't use negative indices.__
#'
#' @param ind.col An optional vector of the column indices that are used.
#' If not specified, all columns are used. __Don't use negative indices.__
#'
#' @param block.size Maximum number of columns read at once. Default is `1000`.
#'
#' @param ncores Number of cores used. Default doesn't use parallelism.
#' @param ncores2 Number of cores used. Default doesn't use parallelism.
#' For this function, use only half of the cores you have.
#'
#' @param fun.scaling A function that returns a named list of
#' `mean` and `sd` for every column, to scale each of their elements
#' such as followed: \deqn{\frac{X_{i,j} - mean_j}{sd_j}}.
#'
#' @param covar.train Matrix of covariables to be added in each model to correct
#' for confounders (e.g. the scores of PCA), corresponding to `ind.train`.
#' Default is `NULL` and corresponds to only adding an intercept to each model.
#' @param covar.row Matrix of covariables to be added in each model to correct
#' for confounders (e.g. the scores of PCA), corresponding to `ind.row`.
#' Default is `NULL` and corresponds to only adding an intercept to each model.
#'
#' @param thr.eigval Threshold to remove "unsignificant" singular vectors.
#' Default is \code{1e-4}.
#'
"_PACKAGE"

################################################################################
