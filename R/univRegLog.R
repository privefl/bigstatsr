#' @title Univariate logistic regression
#' @description Slopes of __univariate__ logistic regressions of each column
#' of a `big.matrix`, with some other associated statistics.
#' Covariates can be added to correct for confounders.
#' @inheritParams bigstatsr-package
#' @param covar Matrix of covariables to be added in each model
#' to correct for confounders (e.g. the scores of PCA). Default
#' is `NULL` and correspond to only adding an Intercept to each model.
#' @param backingpath If the matrix is filebacked, specify the directory
#' where the `big.matrix` is stored.
#' @param y01 Vector of responses. Must be 0s and 1s.
#' @param tol Relative tolerance to assess convergence of the coefficient.
#' Default is `1e-8`.
#' @param maxiter Maximum number of iterations before giving up.
#' Default is `20`.
#' Usually, convergence is reached within 3 or 4 iterations.
#' If there is not convergence,
#' [glm][stats::glm] is used instead for the corresponding column.
#'
#' @return A data.frame with 4 elements:
#' 1. the slopes of each regression,
#' 2. the standard errors of each slope,
#' 3. the z-scores associated with each slope,
#' 4. the p-values associated with each z-score.
#' @example examples/example-univRegLin2.R
#' @seealso [glm][stats::glm]
#' @export
#' @import foreach
big_univRegLog <- function(X, y01, ind.train = seq(nrow(X)),
                           covar = NULL, ncores = 1, backingpath = NULL,
                           tol = 1e-8, maxiter = 20) {
  check_X(X)
  if (ncores > 1) stopifnot(is.shared(X))

  X.desc <- describe(X)
  y.train <- y01[ind.train]
  stopifnot(sort(unique(y.train)) == 0:1)
  n <- length(ind.train)

  if (is.null(covar)) {
    covar <- cbind(rep(0, n), rep(1, n))
  } else {
    covar <- cbind(0, 1, covar)
  }
  stopifnot(n == nrow(covar))

  # no intercept because already in covar
  mod0 <- stats::glm(y.train ~ covar - 1, family = binomial)
  p0 <- mod0$fitted
  w0 <- p0 * (1 - p0)
  z0 <- log(p0 / (1 - p0)) + (y.train - p0) / w0
  rm(mod0, p0)

  PATH <- backingpath
  range.parts <- CutBySize(ncol(X), nb = ncores)

  if (is.seq <- (ncores == 1)) {
    registerDoSEQ()
  } else {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
  }
  res.all <- foreach(ic = seq_len(ncores), .combine = 'cbind') %dopar% {
    lims <- range.parts[ic, ]

    X.part <- sub.big.matrix(X.desc,
                             firstCol = lims[1],
                             lastCol = lims[2],
                             backingpath = PATH)

    # https://www.r-bloggers.com/too-much-parallelism-is-as-bad/
    multi <- (!is.seq) && detect_MRO()
    if (multi) nthreads.save <- RevoUtilsMath::setMKLthreads(1)
    res <- IRLS(X.part@address, covar, y.train, z0, w0,
                ind.train, tol, maxiter)
    if (multi) RevoUtilsMath::setMKLthreads(nthreads.save)

    indNoConv <- which(res$conv >= maxiter)
    if ((l <- length(indNoConv)) > 0) {
      printf(paste("For %d SNPs, IRLS has not converged",
                   "using glm for those instead.\n", sep = "; "), l)

      for (j in indNoConv) {
        mod <- stats::glm(y.train ~ X.part[ind.train, j] + covar - 1,
                          family = binomial)
        coeffs <- summary(mod)$coefficients
        res$betas[j] <- coeffs[1]
        res$std[j] <- coeffs[2]
      }

      res$conv[indNoConv] <- NA
    }

    rbind(res$betas, res$std, res$conv)
  }
  if (!is.seq) parallel::stopCluster(cl)

  z.scores <- res.all[[1]] / res.all[[2]]
  p.values <- 2 * stats::pnorm(abs(z.scores), lower.tail = FALSE)
  data.frame(estim = res.all[[1]], std.err = res.all[[2]], niter = res.all[[3]],
             z.score = z.scores, p.value = p.values)
}
