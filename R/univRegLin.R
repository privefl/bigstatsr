################################################################################

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
#' @import foreach
big_univRegLin <- function(X, y.train, ind.train = seq(nrow(X)),
                           covar.train = NULL, ncores = 1) {
  check_X(X, ncores = ncores)

  n <- length(ind.train)
  stopifnot(n == length(y.train))
  stopifnot(n == nrow(covar.train))

  is.seq <- (ncores == 1)
  if (!is.seq) X.desc <- describe(X)

  SVD <- svd(cbind(rep(1, n), covar.train), nv = 0)
  K <- sum(SVD$d / sqrt(n) > 1e-3)

  range.parts <- CutBySize(ncol(X), nb = ncores)

  if (is.seq) {
    registerDoSEQ()
  } else {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
  }
  res.all <- foreach(ic = seq_len(ncores), .combine = 'rbind') %dopar% {
    lims <- range.parts[ic, ]

    if (is.seq) {
      X.part <- X
    } else {
      X.part <- sub.big.matrix(X.desc, firstCol = lims[1], lastCol = lims[2])
    }

    # https://www.r-bloggers.com/too-much-parallelism-is-as-bad/
    multi <- !is.seq && detect_MRO()
    if (multi) nthreads.save <- RevoUtilsMath::setMKLthreads(1)
    res <- univRegLin5(X.part@address, SVD$u[, 1:K, drop = FALSE],
                       y.train, ind.train)
    if (multi) RevoUtilsMath::setMKLthreads(nthreads.save)

    as.data.frame(res)
  }
  if (!is.seq) parallel::stopCluster(cl)

  t.scores <- res.all$estim / res.all$std.err
  p.values <- 2 * stats::pt(abs(t.scores), df = n - K - 1, lower.tail = FALSE)
  cbind(res.all, t.score = t.scores, p.value = p.values)
}

################################################################################
