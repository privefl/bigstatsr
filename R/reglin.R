################################################################################

#' @title Operations of linear regression
#' @description Operations of \bold{univariate} linear regression
#' (intercept, slope and coefficient of determination)
#' on all the columns of a \code{big.matrix}.
#' @inheritParams bigstatsr-package
#' @return \itemize{
#' \item \code{RsqReg} and \code{RsqClass} return a vector of
#' the coefficient of determination \eqn{R^2} of each regression,
#' \item \code{CoeffsReg} and \code{CoeffsClass} return a matrix of
#' the intercepts (first row) and slopes (second row) of each regression.
#' }
#' @example examples/example.reglin.R
#' @seealso \code{\link[bigmemory]{big.matrix-class}} \code{\link[stats]{lm}}
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
