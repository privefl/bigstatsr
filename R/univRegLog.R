################################################################################

#' @title Univariate logistic regression
#' @description Slopes of __univariate__ logistic regressions of each column
#' of a `big.matrix`, with some other associated statistics.
#' Covariates can be added to correct for confounders.
#' @inheritParams bigstatsr-package
#' @param covar.train Matrix of covariables to be added in each model to correct
#' for confounders (e.g. the scores of PCA), corresponding to `ind.train`.
#' Default is `NULL` and corresponds to only adding an Intercept to each model.
#' @param y01.train Vector of responses, corresponding to `ind.train`.
#' Must be 0s and 1s.
#' @param tol Relative tolerance to assess convergence of the coefficient.
#' Default is `1e-8`.
#' @param maxiter Maximum number of iterations before giving up.
#' Default is `20`. Usually, convergence is reached within 3 or 4 iterations.
#' If there is not convergence,
#' [glm][stats::glm] is used instead for the corresponding column.
#'
#' @return A data.frame with as elements:
#' 1. the slopes of each regression,
#' 2. the standard errors of each slope,
#' 3. the number of iteration for each slope. If is `NA`, this means that the
#' algorithm didn't converge, and [glm][stats::glm] was used instead.
#' 3. the z-scores associated with each slope,
#' 4. the p-values associated with each z-score.
#' @example examples/example-univRegLog.R
#' @seealso [glm][stats::glm]
#' @export
#' @import foreach
big_univRegLog <- function(X, y01.train, ind.train = seq(nrow(X)),
                           covar.train = NULL, ncores = 1,
                           tol = 1e-8, maxiter = 20) {
  check_X(X, ncores = ncores)
  stopifnot(sort(unique(y01.train)) == 0:1)

  is.seq <- (ncores == 1)
  if (!is.seq) X.desc <- describe(X)
  n <- length(ind.train)
  stopifnot(n == length(y01.train))


  if (is.null(covar.train)) {
    covar.train <- cbind(rep(0, n), rep(1, n))
  } else {
    covar.train <- cbind(0, 1, covar.train)
  }
  stopifnot(n == nrow(covar.train))

  # no intercept because already in covar.train
  mod0 <- stats::glm(y01.train ~ covar.train - 1, family = "binomial")
  p0 <- mod0$fitted
  w0 <- p0 * (1 - p0)
  z0 <- log(p0 / (1 - p0)) + (y01.train - p0) / w0
  rm(mod0, p0)

  range.parts <- CutBySize(ncol(X), nb = ncores)

  if (is.seq) {
    registerDoSEQ()
  } else {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
  }
  res.all <- foreach(ic = seq_len(ncores), .combine = 'cbind') %dopar% {
    lims <- range.parts[ic, ]

    if (is.seq) {
      X.part <- X
    } else {
      X.part <- sub.big.matrix(X.desc, firstCol = lims[1], lastCol = lims[2])
    }

    # https://www.r-bloggers.com/too-much-parallelism-is-as-bad/
    multi <- !is.seq && detect_MRO()
    if (multi) nthreads.save <- RevoUtilsMath::setMKLthreads(1)
    res <- IRLS(X.part@address, covar.train, y01.train, z0, w0,
                ind.train, tol, maxiter)
    if (multi) RevoUtilsMath::setMKLthreads(nthreads.save)

    indNoConv <- which(res$conv >= maxiter)
    if ((l <- length(indNoConv)) > 0) {
      printf(paste("For %d columns, IRLS has not converged",
                   "using glm for those instead.\n", sep = "; "), l)

      for (j in indNoConv) {
        mod <- stats::glm(y01.train ~ X.part[ind.train, j] + covar.train - 1,
                          family = "binomial",
                          control = list(epsilon = tol, maxit = 100))
        coeffs <- summary(mod)$coefficients
        res$betas[j] <- coeffs[1]
        res$std[j] <- coeffs[2]
      }

      res$conv[indNoConv] <- NA
    }

    rbind(res$betas, res$std, res$conv)
  }
  if (!is.seq) parallel::stopCluster(cl)

  z.scores <- res.all[1, ] / res.all[2, ]
  p.values <- 2 * stats::pnorm(abs(z.scores), lower.tail = FALSE)
  data.frame(estim = res.all[1, ], std.err = res.all[2, ], niter = res.all[3, ],
             z.score = z.scores, p.value = p.values)
}

################################################################################
