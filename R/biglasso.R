################################################################################

check_biglasso <- function() {
  if (utils::packageVersion("biglasso") != "1.3.1.6670")
    stop(paste0("Please use my fork for now ",
                "(until I merge it with Yaohui Zeng's repo).\n",
                "You can get it via ",
                "'devtools::install_github(\"privefl/biglasso\")'."))
}

################################################################################

#' Sparse linear regression
#'
#' Fit lasso penalized linear regression path for a `big.matrix`.
#' Covariates can be added to correct for confounders.
#' This is a wrapper of a modified version of [biglasso][biglasso::biglasso].
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams biglasso::biglasso
#' alpha penalty nlambda lambda.min dfmax verbose ncores
#'
#' @inherit biglasso::biglasso return
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

  biglasso::COPY_biglasso(X, y.train, ind.train, covar.train,
                          family = "gaussian",
                          screen = "COPY-SSR",
                          ...)
}

################################################################################

#' Sparse logistic regression
#'
#' Fit lasso penalized logistic regression path for a `big.matrix`.
#' Covariates can be added to correct for confounders.
#' This is a wrapper of a modified version of [biglasso][biglasso::biglasso].
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams biglasso::biglasso
#' alpha penalty nlambda lambda.min dfmax verbose ncores
#'
#' @inherit big_spRegLin return seealso references
#'
#' @example
#'
#' @export
big_spRegLog <- function(X, y01.train, ind.train = seq(nrow(X)),
                         covar.train = NULL, ...) {
  check_biglasso()

  biglasso::COPY_biglasso(X, y01.train, ind.train, covar.train,
                          family = "binomial",
                          screen = "COPY-SSR",
                          ...)
}

################################################################################
