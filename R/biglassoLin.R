################################################################################

#' Sparse linear regression
#'
#' Fit lasso penalized linear regression path for a `big.matrix`.
#' Covariates can be added to correct for confounders.
#' __This is a modified version of package of
#' [package biglasso](https://github.com/YaohuiZeng/biglasso)__.
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams COPY_biglasso -X -y.train -ind.train -covar.train -family
#'
#' @inherit COPY_biglasso return
#'
#' @example
#'
#' @seealso [glmnet][glmnet::glmnet] [biglasso][biglasso::biglasso]
#' @references Tibshirani, R., Bien, J., Friedman, J., Hastie, T.,
#' Simon, N., Taylor, J. and Tibshirani, R. J. (2012),
#' Strong rules for discarding predictors in lasso-type problems.
#' Journal of the Royal Statistical Society:
#' Series B (Statistical Methodology), 74: 245–266.
#' \url{http://dx.doi.org/10.1111/j.1467-9868.2011.01004.x}.
#'
#' @export
big_spRegLin <- function(X, y.train, ind.train = seq(nrow(X)),
                         covar.train = NULL, ...) {
  check_biglasso()

  COPY_biglasso(X, y.train, ind.train, covar.train, family = "gaussian", ...)
}

################################################################################
