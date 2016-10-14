################################################################################

#' @title Operations of linear regression
#' @description Operations of \bold{univariate} linear regression
#' (intercept, slope and coefficient of determination)
#' on all the columns of a \code{big.matrix}\cr
#' In the case of classification,
#' weights can be used to make the results independent
#' from the proportion of cases / controls
#' (see parameter \emph{weights} of \code{lm}).
#' @inheritParams bigstatsr-package
#' @param weighted Use weigths? Default is \code{FALSE}.
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
RsqReg <- function(X, y, ind.train = seq(nrow(X))) {
  check_X(X, y, y.type = "reg")

  R_squared(X@address, y, ind.train, rep(1, length(ind.train)))
}

################################################################################

#' @rdname reglin
#' @export
RsqClass <- function(X, y, ind.train = seq(nrow(X)),
                     weighted = FALSE) {
  check_X(X, y, y.type = "class")

  prop.case <- mean(y[ind.train] == 1)
  ratio <- 1 / prop.case - 1
  if (weighted) {
    weights <- ifelse(y[ind.train] > 0, ratio, 1)
  } else {
    weights <- rep(1, length(ind.train))
  }

  R_squared(X@address, y, ind.train, weights)
}

################################################################################

#' @rdname reglin
#' @export
CoeffsReg <- function(X, y, ind.train = seq(nrow(X))) {
  check_X(X, y, y.type = "reg")

  betasRegLin(X@address, y, ind.train, rep(1, length(ind.train)))
}

################################################################################

#' @rdname reglin
#' @export
CoeffsClass <- function(X, y, ind.train = seq(nrow(X)),
                        weighted = FALSE) {
  check_X(X, y, y.type = "class")

  prop.case <- mean(y[ind.train] == 1)
  ratio <- 1 / prop.case - 1
  if (weighted) {
    weights <- ifelse(y[ind.train] > 0, ratio, 1)
  } else {
    weights <- rep(1, length(ind.train))
  }

  betasRegLin(X@address, y, ind.train, weights)
}

################################################################################
