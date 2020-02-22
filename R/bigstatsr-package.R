################################################################################

#' @useDynLib bigstatsr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom methods new
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
#' @param ncores Number of cores used. Default doesn't use parallelism.
#'   You may use [nb_cores].
#'
#' @param fun.scaling A function that returns a named list of
#'   `mean` and `sd` for every column, to scale each of their elements
#'   such as followed: \deqn{\frac{X_{i,j} - mean_j}{sd_j}.}
#'   Default doesn't use any scaling.
#'
#' @param covar.train Matrix of covariables to be added in each model to correct
#' for confounders (e.g. the scores of PCA), corresponding to `ind.train`.
#' Default is `NULL` and corresponds to only adding an intercept to each model.
#' @param covar.row Matrix of covariables to be added in each model to correct
#' for confounders (e.g. the scores of PCA), corresponding to `ind.row`.
#' Default is `NULL` and corresponds to only adding an intercept to each model.
#'
#' @param center Vector of same length of `ind.col` to subtract from columns
#'   of `X`.
#' @param scale Vector of same length of `ind.col` to divide from columns
#'   of `X`.
#'
#' @section Matrix parallelization:
#'   Large matrix computations are made block-wise and won't be parallelized
#'   in order to not have to reduce the size of these blocks.
#'   Instead, you may use [Microsoft R Open](https://mran.microsoft.com/open/)
#'   or OpenBLAS in order to accelerate these block matrix computations.
#'   You can also control the number of cores used with
#'   `bigparallelr::set_blas_ncores()`.
#'
"_PACKAGE"

################################################################################
