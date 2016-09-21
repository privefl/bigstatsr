################################################################################

#' @title Operations of linear regression
#' @description Operations of \bold{univariate} linear regression
#' (intercept, slope and coefficient of determination)
#' on all the columns of a \code{big.matrix}\cr
#' In the case of classification,
#' weights are used to make the results independent
#' from the proportion of cases / controls
#' (see parameter \emph{weights} of \code{lm}).
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
RsqReg <- function(X, y, ind.train = seq(nrow(X))) {
  check_X(X)

  if (length(unique(y[ind.train])) > 2) {
    return(R_squared(X@address, y, ind.train, rep(1, length(ind.train))))
  } else {
    stop(ERROR_REG)
  }
}

################################################################################

#' @rdname reglin
#' @export
RsqClass <- function(X, y, ind.train = seq(nrow(X))) {
  check_X(X)

  if (all(sort(unique(y)) == c(-1, 1))) {
    prop.case <- mean(y[ind.train] == 1)
    ratio <- 1 / prop.case - 1
    weights <- ifelse(y[ind.train] > 0, ratio, 1)
    return(R_squared(X@address, y, ind.train, weights))
  } else {
    stop(ERROR_CLASS)
  }
}

################################################################################

#' @rdname reglin
#' @export
CoeffsReg <- function(X, y, ind.train = seq(nrow(X))) {
  check_X(X)

  if (length(unique(y[ind.train])) > 2) {
    return(betasRegLin(X@address, y, ind.train, rep(1, length(ind.train))))
  } else {
    stop(ERROR_REG)
  }
}

################################################################################

#' @rdname reglin
#' @export
CoeffsClass <- function(X, y, ind.train = seq(nrow(X))) {
  check_X(X)

  if (all(sort(unique(y)) == c(-1, 1))) {
    prop.case <- mean(y[ind.train] == 1)
    ratio <- 1 / prop.case - 1
    weights <- ifelse(y[ind.train] > 0, ratio, 1)
    return(betasRegLin(X@address, y, ind.train, weights))
  } else {
    stop(ERROR_CLASS)
  }
}

################################################################################
