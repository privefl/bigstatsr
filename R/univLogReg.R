################################################################################

univLogReg_sub <- function(X, ind, covar.train, y01.train, z0, w0,
                           ind.train, tol, maxiter) {

  res <- IRLS(X, covar.train, y01.train, z0, w0,
              ind.train, ind, tol, maxiter)

  # using `glm` if not converged
  indNoConv <- which(res$niter >= maxiter | is.nan(res$estim))
  res$niter[indNoConv] <- NA
  for (j in indNoConv) {
    mod <- stats::glm(y01.train ~ X[ind.train, ind[j]] + covar.train[, -1] - 1,
                      family = "binomial",
                      control = list(epsilon = tol, maxit = 100))
    coeffs <- `if`(mod$converged, summary(mod)$coefficients[1, 1:2], c(NA, NA))
    res$estim[j] <- coeffs[1]
    res$std.err[j] <- coeffs[2]
  }

  as.data.frame(res)
}

################################################################################

#' Column-wise logistic regression
#'
#' Slopes of column-wise logistic regressions of each column
#' of a `big.matrix`, with some other associated statistics.
#' Covariates can be added to correct for confounders.
#'
#' If convergence is not reached by the main algorithm for some columns,
#' the corresponding `niter` element is set to `NA` and a message is given.
#' Then, [glm][stats::glm] is used instead for the corresponding column.
#' If it can't converge either, all corresponding estimations are set to `NA`.
#'
#' @inheritParams bigstatsr-package
#'
#' @param tol Relative tolerance to assess convergence of the coefficient.
#' Default is `1e-8`.
#' @param maxiter Maximum number of iterations before giving up.
#' Default is `20`. Usually, convergence is reached within 3 or 4 iterations.
#' If there is not convergence,
#' [glm][stats::glm] is used instead for the corresponding column.
#'
#' @return A data.frame with 4 elements:
#' 1. the slopes of each regression,
#' 2. the standard errors of each slope,
#' 3. the number of iteration for each slope. If is `NA`, this means that the
#' algorithm didn't converge, and [glm][stats::glm] was used instead.
#' 4. the z-scores associated with each slope.
#' This is also an object of class `mhtest`. See `methods(class = "mhtest")`.
#'
#' @example examples/example-univLogReg.R
#'
#' @seealso [glm][stats::glm]
#' @export
big_univLogReg <- function(X, y01.train,
                           ind.train = rows_along(X),
                           ind.col = cols_along(X),
                           covar.train = NULL,
                           ncores = 1,
                           tol = 1e-8,
                           maxiter = 20) {
  check_args()

  n <- length(ind.train)
  covar.train <- cbind(rep(0, n), rep(1, n), covar.train)
  assert_lengths(ind.train, y01.train, rows_along(covar.train))

  # precompute some estimation with only the covariables (and the intercept)
  mod0 <- stats::glm(y01.train ~ covar.train[, -1] - 1, family = "binomial")
  p0 <- mod0$fitted
  w0 <- p0 * (1 - p0)
  z0 <- log(p0 / (1 - p0)) + (y01.train - p0) / w0

  # main computation
  res <- big_parallelize(X = X,
                         p.FUN = univLogReg_sub,
                         p.combine = "rbind",
                         ind = ind.col,
                         ncores = ncores,
                         covar.train = covar.train,
                         y01.train = y01.train,
                         z0 = z0, w0 = w0,
                         ind.train = ind.train,
                         tol = tol, maxiter = maxiter)

  # infos on convergence
  if (l.NA <- sum(is.na(res$niter))) {
    message2(
      "For %d columns, IRLS didn't converge; `glm` was used instead.", l.NA)

    if (nbNA <- sum(is.na(res$estim)))
      warning2("For %d columns, glm didn't converge either.", nbNA)
  }

  res$score <- res$estim / res$std.err
  fun.pred <- function(xtr) {
    lpval <- stats::pnorm(xtr, lower.tail = FALSE, log.p = TRUE)
    (log(2) + lpval) / log(10)
  }

  structure(res,
            class = c("mhtest", "data.frame"),
            transfo = abs,
            predict = fun.pred)
}

################################################################################
