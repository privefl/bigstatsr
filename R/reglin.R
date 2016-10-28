################################################################################

#' @title Operations of linear regression
#' @description Operations of __univariate__ linear regression
#' (intercept, slope and coefficient of determination)
#' on all the columns of a `big.matrix`.
#' @inheritParams bigstatsr-package
#' @return
#' - `RsqRegLin` returns a vector of
#' the coefficient of determination \eqn{R^2} of each regression,
#' - `CoeffsRegLin` returns a matrix of the intercepts (first row)
#' and slopes (second row) of each regression.
#' @example examples/example.reglin.R
#' @seealso [lm][stats::lm]
#' @name reglin
NULL

################################################################################

#' @rdname reglin
#' @export
RsqRegLin <- function(X, y, ind.train = seq(nrow(X))) {
  check_X(X)
  R_squared(X@address, y, ind.train)
}

################################################################################

#' @rdname reglin
#' @export
CoeffsRegLin <- function(X, y, ind.train = seq(nrow(X))) {
  check_X(X)

  res <- betasRegLin(X@address, y, ind.train)
  rownames(res) <- c("Intercepts", "Slopes")
  res
}

################################################################################
