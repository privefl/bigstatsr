################################################################################

#' @import bigmemory
#' @useDynLib bigstatsr
#' @importFrom Rcpp sourceCpp
#'
#' @param X A [big.matrix][big.matrix-class].
#' @param X.desc A [big.matrix.descriptor][big.matrix.descriptor-class].
#' @param X. Either a [big.matrix][big.matrix-class] or
#' a [big.matrix.descriptor][big.matrix.descriptor-class].
#'
#' @param y.train Vector of responses, corresponding to `ind.train`.
#' @param y01.train Vector of responses, corresponding to `ind.train`.
#' Must be 0s and 1s.
#'
#' @param ind.train An optional vector of the row indices that are used,
#' for the training part. If not specified, all rows are used.
#' Don't use negative indices.
#' @param ind.row An optional vector of the row indices that are used.
#' If not specified, all rows are used. Don't use negative indices.
#'
#' @param ind.col An optional vector of the column indices that are used.
#' If not specified, all columns are used. Don't use negative indices.
#'
#' @param block.size Maximum number of columns read at once.
#' Default is `1000`. This parameter controls the trade-off between
#' memory usage and speed. Basically, the more you can load at once,
#' the quicker will be the execution time, at the expense of memory usage.
#'
#' @param ncores Number of cores used. Default doesn't use parallelism.
#' @param ncores2 Number of cores used. Default doesn't use parallelism.
#' For this function, use only half of the cores you have.
#'
#' @param fun.scaling A function that returns a named list of
#' __`mean`__ and __`sd`__ for every column, to scale each of their elements
#' such as followed: \deqn{\frac{X_{i,j} - mean_j}{sd_j}}.
#'
#' @param covar.train Matrix of covariables to be added in each model to correct
#' for confounders (e.g. the scores of PCA), corresponding to `ind.train`.
#' Default is `NULL` and corresponds to only adding an intercept to each model.
#' @param covar.row Matrix of covariables to be added in each model to correct
#' for confounders (e.g. the scores of PCA), corresponding to `ind.row`.
#' Default is `NULL` and corresponds to only adding an intercept to each model.
#'
#' @param use.Eigen Should the `Eigen` library be used
#' for matrix computations? Default tries to detect MRO. See details.
#' @details For matrix computations, using \code{Eigen} library is faster.
#' However, if you link \code{R} with an optimized math library,
#' using \code{R}'s base operations is even much faster.
#'
#' For example, you can easily link \code{R} with the
#' \href{https://software.intel.com/en-us/intel-mkl}{Intel®
#' Math Kernel Library} (Intel® MKL) through
#' \href{https://mran.revolutionanalytics.com/open/}{Microsoft
#' R Open} (MRO). It really improves performance
#' of \code{R} and \code{RcppArmadillo} matrix computations,
#' yet not the ones of \code{RcppEigen} (at least not directly).
#'
#' So,
#' 1. `Eigen` should be prefered if you don't change anything,
#' 2. base `R` should be prefered if you use MRO,
#' 3. `Eigen` may be prefered if you manage to link `RcppEigen` with the MKL
#' (please \href{mailto:florian.prive.21@gmail.com}{contact me} if you do!).
"_PACKAGE"

################################################################################
