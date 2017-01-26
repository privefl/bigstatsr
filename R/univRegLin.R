#' @title Column-wise linear regression
#' @description Slopes of __univariate__ linear regressions of each column
#' of a `big.matrix`, with some other associated statistics.
#' Covariates can be added to correct for confounders.
#'
#' @inheritParams bigstatsr-package
#'
#' @return A data.frame with 4 elements:
#' 1. the slopes of each regression,
#' 2. the standard errors of each slope,
#' 3. the t-scores associated with each slope,
#' 4. the p-values associated with each t-score.
#' @example examples/example-univRegLin2.R
#' @seealso [lm][stats::lm]
#' @export
big_univRegLin <- function(X, y.train, ind.train = seq(nrow(X)),
                           covar.train = NULL) {
  check_X(X)

  n <- length(ind.train)
  stopifnot(n == length(y.train))

  if (is.null(covar.train)) {
    K <- 2
    res <- univRegLin(X@address, y = y.train, rowInd = ind.train)
  } else {
    covar.train <- cbind(0, 1, covar.train)
    stopifnot(n == nrow(covar.train))
    K <- ncol(covar.train)
    res <- univRegLin2(X@address, covar = covar.train, y = y.train,
                       rowInd = ind.train)
  }
  t.scores <- res[[1]] / res[[2]]
  p.values <- 2 * stats::pt(abs(t.scores), df = n - K, lower.tail = FALSE)
  data.frame(res, t.score = t.scores, p.value = p.values)
}
