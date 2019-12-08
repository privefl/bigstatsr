################################################################################

univLinReg_sub <- function(X, ind, U, y.train, ind.train) {

  as.data.frame(univLinReg5(X, U, y.train, ind.train, ind))
}

################################################################################

#' Column-wise linear regression
#'
#' Slopes of column-wise linear regressions of each column
#' of a Filebacked Big Matrix, with some other associated statistics.
#' Covariates can be added to correct for confounders.
#'
#' @inheritParams bigstatsr-package
#' @param thr.eigval Threshold to remove "insignificant" singular vectors.
#' Default is \code{1e-4}.
#'
#' @return A data.frame with 3 elements:
#' 1. the slopes of each regression,
#' 2. the standard errors of each slope,
#' 3. the t-scores associated with each slope.
#' This is also an object of class `mhtest`. See `methods(class = "mhtest")`.
#'
#' @example examples/example-univLinReg.R
#' @seealso [lm][stats::lm]
#'
#' @export
#'
big_univLinReg <- function(X, y.train,
                           ind.train = rows_along(X),
                           ind.col = cols_along(X),
                           covar.train = NULL,
                           thr.eigval = 1e-4,
                           ncores = 1) {

  check_args()

  n <- length(ind.train)
  covar.train <- cbind(rep(1, n), covar.train)
  assert_lengths(ind.train, y.train, rows_along(covar.train))
  stopifnot(n > ncol(covar.train))

  # get SVD of covar
  SVD <- svd(covar.train, nv = 0)
  eigval.scaled <- SVD$d / (sqrt(n) + sqrt(ncol(covar.train)) - 1)
  K <- sum(eigval.scaled > thr.eigval)

  # main computation
  res <- big_parallelize(X = X,
                         p.FUN = univLinReg_sub,
                         p.combine = "rbind",
                         ind = ind.col,
                         ncores = ncores,
                         U = SVD$u[, 1:K, drop = FALSE],
                         y.train = y.train,
                         ind.train = ind.train)

  res$score <- res$estim / res$std.err
  fun.pred <- eval(parse(text = sprintf(
    "function(xtr) {
       lpval <- stats::pt(xtr, df = %d, lower.tail = FALSE, log.p = TRUE)
       (log(2) + lpval) / log(10)
     }", n - K - 1)))
  environment(fun.pred) <- baseenv()

  structure(res,
            class = c("mhtest", "data.frame"),
            transfo = abs,
            predict = fun.pred)
}

################################################################################
