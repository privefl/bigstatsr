################################################################################

null_pred <- function(var0, y, base, family) {

  fit <- stats::glm.fit(var0, y, offset = base, intercept = FALSE,
                        family = switch(family,
                                        gaussian = stats::gaussian(),
                                        binomial = stats::binomial()))

  `if`(anyNA(fit$coefficients), stop2("Problem is singular."), fit)
}

################################################################################

summaries <- function(X, y_diff.train, ind.train, ind.col, ind.sets, K,
                      covar.train = matrix(0, length(ind.train), 0)) {

  summ <- bigsummaries(X, ind.train, ind.col, covar.train, y_diff.train,
                       ind.sets, K)

  all <- colSums(summ)
  SUM_X  <- sweep(-matrix(summ[, , 1], K), 2, all[, 1], '+')
  SUM_XX <- sweep(-matrix(summ[, , 2], K), 2, all[, 2], '+')
  SUM_XY <- sweep(-matrix(summ[, , 3], K), 2, all[, 3], '+')
  SUM_Y <- tapply(seq_along(ind.sets), factor(ind.sets, levels = 1:K),
                  function(ind) sum(y_diff.train[-ind]))

  n.sets      <- length(ind.train) - table(ind.sets)
  center.sets <- sweep(SUM_X, 1, n.sets, '/')
  scale.sets  <- sqrt(sweep(SUM_XX, 1, n.sets, '/') - center.sets^2)
  keep        <- (colSums(scale.sets > 1e-6) == K)
  resid.sets  <- (SUM_XY - sweep(center.sets, 1, SUM_Y, '*')) /
    sweep(scale.sets, 1, n.sets, '*')

  list(keep = keep, center = center.sets, scale = scale.sets, resid = resid.sets)
}

################################################################################
###             This is a modified version from package biglasso             ###
###                  https://github.com/YaohuiZeng/biglasso                  ###
################################################################################

#' Train one model
#'
#' @return A named list with following variables:
#'   \item{intercept}{A vector of intercepts, corresponding to each lambda.}
#'   \item{beta}{The vector of coefficients that minimized the loss on the
#'     validation set.}
#'   \item{iter}{A vector of length `nlambda` containing the number of
#'     iterations until convergence at each value of `lambda`.}
#'   \item{lambda}{The sequence of regularization parameter values in the path.}
#'   \item{alpha}{Input parameter.}
#'   \item{loss}{A vector containing either the residual sum of squares
#'     (for linear models) or negative log-likelihood (for logistic models)
#'     of the fitted model at each value of `lambda`.}
#'   \item{loss.val}{A vector containing the loss for the corresponding
#'     validation set.}
#'   \item{message}{Reason the fitting has stopped.}
#'   \item{nb_active}{The number of active (non-zero) variables along the
#'     regularization path.}
#'   \item{nb_candidate}{The number of candidate variables (used in the gradient
#'     descent) along the regularization path.}
#'   \item{ind.train}{Indices of training set.}
#'
#' @keywords internal
#'
COPY_biglasso_part <- function(X, y.train, ind.train, ind.col, covar.train,
                               family, lambda, center, scale, resid, alpha,
                               eps, max.iter, dfmax,
                               ind.val, covar.val, y.val, n.abort, nlam.min,
                               base.train, base.val, pf) {

  assert_lengths(y.train, base.train, ind.train, rows_along(covar.train))
  assert_lengths(y.val, base.val, ind.val, rows_along(covar.val))
  assert_lengths(c(ind.col, cols_along(covar.train)), center, scale, resid)
  stopifnot(length(intersect(ind.train, ind.val)) == 0)

  ## fit model
  if (family == "gaussian") {

    y.train <- y.train - base.train
    a <- y.train.mean <- mean(y.train)

    res <- COPY_cdfit_gaussian_hsr(
      X, y.train - y.train.mean, ind.train, ind.col, covar.train,
      lambda, center, scale, pf, resid, alpha, eps, max.iter, dfmax,
      ind.val, covar.val, y.val - base.val - y.train.mean, n.abort, nlam.min)

  } else if (family == "binomial") {

    res <- COPY_cdfit_binomial_hsr(
      X, y.train, base.train, ind.train, ind.col, covar.train,
      lambda, center, scale, pf, resid, alpha, eps, max.iter, dfmax,
      ind.val, covar.val, y.val, base.val, n.abort, nlam.min)

    a <- res[[1]]
    res <- res[-1]

  } else {
    stop("Current version only supports Gaussian or Binominal response!")
  }

  b <- res[[1]]
  loss <- res[[2]]
  iter <- res[[3]]
  loss.val <- res[[4]]
  mess <- res[[5]]
  nb_active <- res[[6]]
  nb_candidate <- res[[7]]

  ## Eliminate saturated lambda values, if any
  ind <- !is.na(loss.val)

  if (isTRUE(any(iter >= max.iter)))
    warning2("Algorithm failed to converge for some values of lambda")

  ## Unstandardize coefficients:
  bb <- b / scale
  aa <- a - sum(center * bb)

  ## Output
  structure(list(
    intercept = aa,
    beta = bb,
    iter = iter[ind],
    lambda = lambda[ind],
    alpha = alpha,
    loss = loss[ind] / length(ind.train),
    loss.val = loss.val[ind] / length(ind.val),
    message = mess,
    nb_active = nb_active[ind],
    nb_candidate = nb_candidate[ind],
    ind.train = ind.train
  ), class = "big_sp")
}

################################################################################

#' Sparse regression path
#'
#' Fit solution paths for linear or logistic regression models penalized by
#' lasso (alpha = 1) or elastic-net (1e-4 < alpha < 1) over a grid of values
#' for the regularization parameter lambda.
#'
#' The objective function for linear regression (\code{family = "gaussian"}) is
#' \deqn{\frac{1}{2n}\textrm{RSS} + \textrm{penalty},} for logistic regression
#' (\code{family = "binomial"}) it is \deqn{-\frac{1}{n} loglike +
#' \textrm{penalty}.}
#'
#' @param family Either "gaussian" (linear) or "binomial" (logistic).
#' @param alphas The elastic-net mixing parameter that controls the relative
#' contribution from the lasso (l1) and the ridge (l2) penalty. The penalty is
#' defined as \deqn{ \alpha||\beta||_1 + (1-\alpha)/2||\beta||_2^2.}
#' \code{alpha = 1} is the lasso penalty and \code{alpha} in between `0`
#' (`1e-4`) and `1` is the elastic-net penalty. Default is `1`. **You can
#' pass multiple values, and only one will be used (optimized by grid-search).**
#' @param lambda.min The smallest value for lambda, **as a fraction of
#' lambda.max**. Default is `.0001` if the number of observations is larger than
#' the number of variables and `.001` otherwise.
#' @param nlambda The number of lambda values. Default is `200`.
#' @param eps Convergence threshold for inner coordinate descent.
#' The algorithm iterates until the maximum change in the objective after any
#' coefficient update is less than `eps` times the null deviance.
#' Default value is `1e-5`.
#' @param max.iter Maximum number of iterations. Default is `1000`.
#' @param dfmax Upper bound for the number of nonzero coefficients. Default is
#' `50e3` because, for large data sets, computational burden may be
#' heavy for models with a large number of nonzero coefficients.
#' @param K Number of sets used in the Cross-Model Selection and Averaging
#'   (CMSA) procedure. Default is `10`.
#' @param ind.sets Integer vectors of values between `1` and `K` specifying
#'   which set each index of the training set is in. Default randomly assigns
#'   these values but it can be useful to set this vector for reproducibility,
#'   or if you want to refine the grid-search over `alphas` using the same sets.
#' @param warn Deprecated. Now return the reason of completion as `$message`.
#' @param return.all Deprecated. Now always return all models.
#' @param nlam.min Minimum number of lambda values to investigate. Default is `50`.
#' @param n.abort Number of lambda values for which prediction on the validation
#'   set must decrease before stopping. Default is `10`.
#' @param base.train Vector of base predictions. Model will be learned starting
#'   from these predictions. This can be useful if you want to previously fit
#'   a model with large-effect variables that you don't want to penalize.
#'   **Don't forget to add those predictions when you use `predict`
#'   and make sure you don't use `proba = TRUE` when you do so.**
#' @param pf.X A multiplicative factor for the penalty applied to each coefficient.
#'   If supplied, `pf.X` must be a numeric vector of the same length as `ind.col`.
#'   Default is all `1`. The purpose of `pf.X` is to apply differential
#'   penalization if some coefficients are thought to be more likely than others
#'   to be in the model. Setting SOME to 0 allows to have unpenalized coefficients.
#' @param pf.covar Same as `pf.X`, but for `covar.train`.
#'
#' @keywords internal
#'
COPY_biglasso_main <- function(X, y.train, ind.train, ind.col, covar.train,
                               family = c("gaussian", "binomial"),
                               alphas = 1,
                               K = 10,
                               ind.sets = NULL,
                               nlambda = 200,
                               lambda.min = `if`(n > p, .0001, .001),
                               nlam.min = 50,
                               n.abort = 10,
                               base.train = NULL,
                               pf.X = NULL,
                               pf.covar = NULL,
                               eps = 1e-5,
                               max.iter = 1000,
                               dfmax = 50e3,
                               return.all = FALSE,
                               warn = FALSE,
                               ncores = 1) {

  if (!missing(warn)) warning2("Parameter 'warn' is deprecated.")
  if (!missing(return.all)) warning2("Parameter 'return.all' is deprecated.")

  family <- match.arg(family)
  dfmax <- min(dfmax, .Machine$integer.max - 10L)

  n <- length(ind.train)
  if (is.null(covar.train)) covar.train <- matrix(0, n, 0)
  if (is.null(base.train))   base.train <- rep(0, n)
  if (is.null(ind.sets))       ind.sets <- sample(rep_len(1:K, n))
  assert_lengths(y.train, ind.train, rows_along(covar.train), base.train, ind.sets)

  p1 <- length(ind.col); p2 <- ncol(covar.train); p <- p1 + p2
  if (is.null(pf.X))     pf.X     <- rep(1, p1)
  if (is.null(pf.covar)) pf.covar <- rep(1, p2)
  assert_lengths(pf.X,     ind.col)
  assert_lengths(pf.covar, cols_along(covar.train))

  if (any(alphas < 1e-4 | alphas > 1)) stop("alpha must be between 1e-4 and 1.")

  if (nlambda < 2) stop("nlambda must be at least 2")

  if (class(y.train) != "numeric")
    tryCatch(y.train <- as.numeric(y.train), error = function(e)
      stop("y.train must numeric or able to be coerced to numeric"))


  # variables that are not penalized + base prediction
  pf <- c(pf.X, pf.covar); assert_all(pf >= 0)
  ind0.X <- which(pf.X == 0)
  ind0.covar <- which(pf.covar == 0)
  var0.train <- cbind(
    1,
    X[ind.train, ind.col[ind0.X], drop = FALSE],
    covar.train[, ind0.covar, drop = FALSE]
  )
  fit <- null_pred(var0.train, y.train, base.train, family)
  y_diff.train <- y.train - fit$fitted.values
  base.train <- fit$linear.predictors
  beta0 <- fit$coef[1]
  beta <- rep(0, p)
  beta[ind0.X] <- head(fit$coef[-1], length(ind0.X))
  beta[p1 + ind0.covar] <- tail(fit$coef, length(ind0.covar))


  # Get summaries
  ## Get also for covariables
  summaries.covar <- summaries(X, y_diff.train, ind.train, integer(0),
                               ind.sets, K, covar.train)
  if (!all(summaries.covar$keep))
    stop("Please use covariables with some variation.")

  ## Parallelize over columns
  list_summaries <- big_parallelize(
    X, p.FUN = function(X, ind, y_diff.train, ind.train, ind.sets, K) {
      summaries(X, y_diff.train, ind.train, ind, ind.sets, K)
    }, ncores = ncores, ind = ind.col, y_diff.train = y_diff.train,
    ind.train = ind.train, ind.sets = ind.sets, K = K)

  keep <- do.call('c', lapply(list_summaries, function(x) x[["keep"]]))
  pf.keep <- pf[keep]


  ## fit models
  if (ncores == 1) {
    registerDoSEQ()
  } else {
    cluster_type <- getOption("bigstatsr.cluster.type")
    cl <- parallel::makeCluster(ncores, type = cluster_type)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  alphas <- sort(alphas)
  cross.res <- foreach(alpha = alphas) %:% foreach(ic = 1:K) %dopar% {

    in.val <- (ind.sets == ic)

    ## Merge summaries
    center <- do.call('c', lapply(list_summaries, function(x) x[["center"]][ic, ]))[keep]
    scale  <- do.call('c', lapply(list_summaries, function(x) x[["scale"]][ic, ]))[keep]
    resid  <- do.call('c', lapply(list_summaries, function(x) x[["resid"]][ic, ]))[keep]
    # keep.covar <- summaries.covar$keep ## should all be TRUE
    center <- c(center, summaries.covar[["center"]][ic, ])
    scale  <- c(scale,  summaries.covar[["scale"]][ic, ])
    resid  <- c(resid,  summaries.covar[["resid"]][ic, ])
    ## Compute lambdas of the path
    lambda.max <- max(abs(resid / pf.keep)[pf.keep != 0]) / alpha
    lambda <- exp(
      seq(log(lambda.max), log(lambda.max * lambda.min), length.out = nlambda))

    res <- COPY_biglasso_part(
      X, y.train = y.train[!in.val],
      ind.train = ind.train[!in.val],
      ind.col = ind.col[keep],
      covar.train = covar.train[!in.val, , drop = FALSE],
      family, lambda, center, scale, resid, alpha, eps, max.iter, dfmax,
      ind.val = ind.train[in.val],
      covar.val = covar.train[in.val, , drop = FALSE],
      y.val = y.train[in.val],
      n.abort, nlam.min,
      # base fitting
      base.train = base.train[!in.val],
      base.val = base.train[in.val],
      pf.keep
    )
    # Add first solution
    res$intercept <-  res$intercept + beta0
    res$beta <- res$beta + beta
    res
  }

  structure(
    cross.res,
    class = "big_sp_list",
    family = family,
    alphas = alphas,
    ind.col = ind.col[keep],
    ind.sets = ind.sets,
    pf = pf.keep
  )
}

################################################################################

#' Sparse linear regression
#'
#' Fit lasso penalized linear regression path for a Filebacked Big Matrix.
#' Covariates can be added to correct for confounders.
#'
#' __This is a modified version of one function of
#' [package biglasso](https://github.com/YaohuiZeng/biglasso)__.
#' It adds the possibility to train models with covariables and use many
#' types of `FBM` (not only `double` ones).
#' Yet, it only corresponds to `screen = "SSR"` (Sequential Strong Rules).
#'
#' Also, to remove the choice of the lambda parameter, we introduce the
#' Cross-Model Selection and Averaging (CMSA) procedure:
#' 1. This function separates the training set in `K` folds (e.g. 10).
#' 2. __In turn__,
#'     - each fold is considered as an inner validation set and the others
#'       (K - 1) folds form an inner training set,
#'     - the model is trained on the inner training set and the corresponding
#'       predictions (scores) for the inner validation set are computed,
#'     - the vector of scores which maximizes log-likelihood is determined,
#'     - the vector of coefficients corresponding to the previous vector of
#'       scores is chosen.
#' 3. The `K` resulting vectors of coefficients can then be combined into one
#' vector (see [get_beta]) or you can just combine the predictions
#' (e.g. using `predict` followed by `rowMeans`).
#'
#' @inheritParams bigstatsr-package
#' @inheritParams COPY_biglasso_main
#' @inheritDotParams COPY_biglasso_main lambda.min eps max.iter warn return.all
#'
#' @return Return an object of class `big_sp_list` (a list of `length(alphas)`
#'   x `K`) that has 3 methods `predict`, `summary` and `plot`.
#'
#' @example examples/example-spLinReg.R
#'
#' @seealso [glmnet][glmnet::glmnet] [biglasso][biglasso::biglasso]
#' @references
#' Tibshirani, R., Bien, J., Friedman, J., Hastie, T.,
#' Simon, N., Taylor, J. and Tibshirani, R. J. (2012),
#' Strong rules for discarding predictors in lasso-type problems.
#' Journal of the Royal Statistical Society:
#' Series B (Statistical Methodology), 74: 245–266.
#' \url{https://doi.org/10.1111/j.1467-9868.2011.01004.x}.
#'
#' Zeng, Y., and Breheny, P. (2016). The biglasso Package: A Memory- and
#' Computation-Efficient Solver for Lasso Model Fitting with Big Data in R.
#' arXiv preprint arXiv:1701.05936. \url{https://arxiv.org/abs/1701.05936}.
#'
#' @export
big_spLinReg <- function(X, y.train,
                         ind.train = rows_along(X),
                         ind.col = cols_along(X),
                         covar.train = NULL,
                         base.train = NULL,
                         pf.X = NULL,
                         pf.covar = NULL,
                         alphas = 1,
                         K = 10,
                         ind.sets = NULL,
                         nlambda = 200,
                         nlam.min = 50,
                         n.abort = 10,
                         dfmax = 50e3,
                         ncores = 1,
                         ...) {

  check_args()

  args <- c(as.list(environment()), list(...))
  args[["family"]] <- "gaussian"

  do.call(COPY_biglasso_main, args)
}

################################################################################

#' Sparse logistic regression
#'
#' @inheritParams bigstatsr-package
#' @inheritParams COPY_biglasso_main
#' @inheritDotParams COPY_biglasso_main lambda.min eps max.iter warn return.all
#'
#' @inherit big_spLinReg return description details seealso references
#'
#' @example examples/example-spLogReg.R
#'
#' @export
big_spLogReg <- function(X, y01.train,
                         ind.train = rows_along(X),
                         ind.col = cols_along(X),
                         covar.train = NULL,
                         base.train = NULL,
                         pf.X = NULL,
                         pf.covar = NULL,
                         alphas = 1,
                         K = 10,
                         ind.sets = NULL,
                         nlambda = 200,
                         nlam.min = 50,
                         n.abort = 10,
                         dfmax = 50e3,
                         ncores = 1,
                         ...) {

  check_args()

  args <- c(as.list(environment()), list(...))
  args[["y.train"]]   <- y01.train
  args[["y01.train"]] <- NULL
  args[["family"]]    <- "binomial"

  do.call(COPY_biglasso_main, args)
}

################################################################################

#' Combine sets of coefficients
#'
#' @param method Method for combining vectors of coefficients. The default uses
#'   the [geometric median](https://en.wikipedia.org/wiki/Geometric_median).
#' @param betas Matrix of coefficient vectors to be combined.
#'
#' @return A vector of resulting coefficients.
#' @export
get_beta <- function(betas, method = c("geometric-median",
                                       "mean-wise",
                                       "median-wise")) {

  method <- match.arg(method)

  if (method == "geometric-median") {
    # Weiszfeld's algorithm
    # probabilistically impossible to not converge in this situation?
    b.old <- rowMeans(betas)

    repeat {
      norm <- sqrt(colSums((betas - b.old)^2))
      b.new <- rowSums(sweep(betas, 2, norm, "/")) / sum(1 / norm)
      dif <- max(abs(b.new - b.old))
      if (dif < 1e-10) break
      b.old <- b.new
    }

    b.new
  } else if (method == "mean-wise") {
    rowMeans(betas)
  } else if (method == "median-wise") {
    apply(betas, 1, stats::median)
  } else {
    stop("This method is not implemented.")
  }
}

################################################################################
