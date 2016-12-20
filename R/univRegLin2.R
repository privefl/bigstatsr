#' @title Univariate linear regression
#' @description Slopes of __univariate__ linear regressions of each column
#' of a `big.matrix`, with some other associated statistics.
#' Covariates can be added to correct for confounders.
#' @inheritParams bigstatsr-package
#' @param covar Matrix of covariables to be added in each model
#' to correct for confounders (e.g. the scores of PCA).
#' @return A data.frame with 4 elements:
#' 1. the slopes of each regression,
#' 2. the standard errors of each slope,
#' 3. the t-scores associated with each slope,
#' 4. the p-values associated with each t-score.
#' @example examples/example-univRegLin2.R
#' @seealso [lm][stats::lm]
#' @export
big_univRegLin <- function(X, y, ind.train = seq(nrow(X)), covar = NULL) {
  check_X(X)

  y.train <- y[ind.train]
  n <- length(ind.train)

  if (is.null(covar)) {
    covar <- cbind(rep(0, n), rep(1, n))
  } else {
    covar <- cbind(0, 1, covar)
  }
  stopifnot(n == nrow(covar))
  K <- ncol(covar)

  res <- univRegLin2(X@address, covar = covar, y = y, rowInd = ind.train)
  t.scores <- res$betas / res$std
  p.values <- 2 * pt(abs(t.scores), df = n - K, lower.tail = FALSE)
  data.frame(res, t.scores = t.scores, p.values = p.values)
}
