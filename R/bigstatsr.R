################################################################################

#' @useDynLib bigstatsr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import foreach
#'
#' @param X A [FBM][FBM-class].
#' @param X.code A [FBM.code256][FBM.code256-class].
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
#' @param block.size Maximum number of columns read at once.
#'   Default uses [block_size].
#'
#' @param fun.scaling A function that returns a named list of
#' `mean` and `sd` for every column, to scale each of their elements
#' such as followed: \deqn{\frac{X_{i,j} - mean_j}{sd_j}.}
#'
#' @param covar.train Matrix of covariables to be added in each model to correct
#' for confounders (e.g. the scores of PCA), corresponding to `ind.train`.
#' Default is `NULL` and corresponds to only adding an intercept to each model.
#' @param covar.row Matrix of covariables to be added in each model to correct
#' for confounders (e.g. the scores of PCA), corresponding to `ind.row`.
#' Default is `NULL` and corresponds to only adding an intercept to each model.
#'
"_PACKAGE"

################################################################################
