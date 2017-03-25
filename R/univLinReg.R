################################################################################

univLinReg_sub <- function(X., ind, U, y.train, ind.train) {
  X <- attach.BM(X.)
  as.data.frame(univLinReg5(X, U, y.train, ind.train, ind))
}

################################################################################

#' Column-wise linear regression
#'
#' Slopes of column-wise linear regressions of each column
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
#'
#' @example examples/example-univLinReg.R
#'
#' @seealso [lm][stats::lm]
#' @export
big_univLinReg <- function(X., y.train,
                           ind.train = rows_along(X.),
                           ind.col = cols_along(X.),
                           covar.train = NULL,
                           ncores2 = 1,
                           thr.eigval = 1e-4) {
  n <- length(ind.train)
  stopifnot(n == length(y.train))
  covar.train <- cbind(rep(1, n), covar.train)
  stopifnot(n == nrow(covar.train))

  # get SVD of covar
  SVD <- svd(covar.train, nv = 0)
  K <- sum(SVD$d / sqrt(n) > thr.eigval)

  # main computation
  res <- big_parallelize(X. = X.,
                         p.FUN = univLinReg_sub,
                         p.combine = 'rbind',
                         ind = ind.col,
                         ncores = ncores2,
                         U = SVD$u[, 1:K, drop = FALSE],
                         y.train = y.train,
                         ind.train = ind.train)

  res$score <- res$estim / res$std.err
  fun.pred <- eval(parse(text = sprintf(
    "function(xtr) {
       lpval <- stats::pt(xtr, df = %d, lower.tail = FALSE, log.p = TRUE)
       (log(2) + lpval) / log(10)
     }", n - K - 1)))

  structure(res,
            class = c("mhtest", "data.frame"),
            transfo = abs,
            predict = fun.pred)
}

################################################################################
