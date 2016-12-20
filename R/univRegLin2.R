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

  n <- length(ind.train)

  if (is.null(covar)) {
    K <- 2
    res <- univRegLin(X@address, y = y, rowInd = ind.train)
  } else {
    covar <- cbind(0, 1, covar)
    stopifnot(n == nrow(covar))
    K <- ncol(covar)
    res <- univRegLin2(X@address, covar = covar, y = y[ind.train],
                       rowInd = ind.train)
  }
  t.scores <- res[[1]] / res[[2]]
  p.values <- 2 * pt(abs(t.scores), df = n - K, lower.tail = FALSE)
  data.frame(res, t.score = t.scores, p.value = p.values)
}
