################################################################################

#' @title Univariate linear regression
#' @description Results of __univariate__ linear regressions
#' (intercept, slope and coefficient of determination)
#' of each column of a `big.matrix`.
#' @inheritParams bigstatsr-package
#' @return A matrix with 3 rows:
#' 1. the intercepts of each regression,
#' 2. the slopes of each regression,
#' 3. the coefficients of determination \eqn{R^2} of each regression.
#' @example examples/example-univRegLin.R
#' @seealso [lm][stats::lm]
#' @export
big_univRegLin <- function(X, y, ind.train = seq(nrow(X))) {
  check_X(X)

  res <- univRegLin(X@address, y, ind.train)
  rownames(res) <- c("Intercepts", "Slopes", "R2")
  res
}

################################################################################
