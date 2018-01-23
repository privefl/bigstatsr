################################################################################

summaries <- function(X, y.train, ind.train, ind.col,
                      covar.train = matrix(0, length(ind.train), 0),
                      ind.sets, K) {

  assert_lengths(ind.train, ind.sets)

  tmp <- bigsummaries(X, ind.train, ind.col, covar.train, y.train, ind.sets, K)

  all <- colSums(tmp)
  n.sets <- length(ind.train) - table(ind.sets)
  SUM_X  <- sweep(-tmp[, , 1], 2, all[, 1], '+')
  SUM_XX <- sweep(-tmp[, , 2], 2, all[, 2], '+')
  SUM_XY <- sweep(-tmp[, , 3], 2, all[, 3], '+')
  SUM_Y  <- sweep(-tmp[, , 4], 2, all[, 4], '+')

  center.sets <- sweep(SUM_X, 1, n.sets, '/')
  scale.sets  <- sqrt(sweep(SUM_XX, 1, n.sets, '/') - center.sets^2)
  keep        <- (colSums(scale.sets > 1e-6) == K)
  resid.sets  <- (SUM_XY - center.sets * SUM_Y) /
    sweep(scale.sets, 1, n.sets, '*')

  list(keep = keep, center = center.sets, scale = scale.sets, resid = resid.sets)
}

################################################################################
###             This is a modified version from package biglasso             ###
###                  https://github.com/YaohuiZeng/biglasso                  ###
################################################################################

COPY_biglasso_part <- function(X, y.train, ind.train, ind.col, covar.train,
                               family, lambda, center, scale, resid, alpha,
                               eps, max.iter, dfmax, warn,
                               ind.val, covar.val, y.val, n.abort, nlam.min) {

  assert_lengths(y.train, ind.train, rows_along(covar.train))
  assert_lengths(y.val, ind.val, rows_along(covar.val))
  assert_lengths(ind.col, center, scale, resid)
  stopifnot(length(intersect(ind.train, ind.val)) == 0)

  ## fit model
  if (family == "gaussian") {

    res <- COPY_cdfit_gaussian_hsr(
      X, y.train - mean(y.train), ind.train, ind.col, covar.train,
      lambda, center, scale, resid, alpha, eps, max.iter, dfmax, warn,
      ind.val, covar.val, y.val, n.abort, nlam.min)

    a <- rep(mean(y.train), nlambda)
    b <- Matrix(res[[1]], sparse = TRUE)
    loss <- res[[2]]
    iter <- res[[3]]

  } else if (family == "binomial") {

    res <- COPY_cdfit_binomial_hsr(
      X, y.train, ind.train, ind.col, covar.train,
      lambda, center, scale, resid, alpha, eps, max.iter, dfmax, warn,
      ind.val, covar.val, y.val, n.abort, nlam.min)

    a <- res[[1]]
    b <- Matrix(res[[2]], sparse = TRUE)
    loss <- res[[3]]
    iter <- res[[4]]
    metric <- res[[5]]

  } else {
    stop("Current version only supports Gaussian or Binominal response!")
  }

  ## Eliminate saturated lambda values, if any
  ind <- !is.na(iter)
  a <- a[ind]
  b <- b[, ind, drop = FALSE]
  iter <- iter[ind]
  lambda <- lambda[ind]
  loss <- loss[ind]
  metric <- metric[ind]

  if (warn && any(iter >= max.iter))
    warning("Algorithm failed to converge for some values of lambda")

  ## Unstandardize coefficients:
  bb <- b / scale
  aa <- a - as.numeric(crossprod(center, bb))

  ## Names
  names(aa) <- colnames(bb) <- signif(lambda, digits = 4)

  ## Output
  structure(list(
    intercept = aa,
    beta = bb,
    iter = iter,
    lambda = lambda,
    family = family,
    alpha = alpha,
    loss = loss,
    metric = metric,
    ind.train = ind.train,
    ind.col = ind.col
  ), class = "big_sp")
}

################################################################################

#' Sparse regression path
#'
#' Fit solution paths for linear or logistic regression models penalized by
#' lasso (alpha = 1) or elastic-net (1e-6 < alpha < 1) over a grid of values
#' for the regularization parameter lambda.
#'
#' The objective function for linear regression (\code{family = "gaussian"}) is
#' \deqn{\frac{1}{2n}\textrm{RSS} + \textrm{penalty},} for logistic regression
#' (\code{family = "binomial"}) it is \deqn{-\frac{1}{n} loglike +
#' \textrm{penalty}.}
#'
#' @param family Either "gaussian" or "binomial", depending on the response.
#' @param alpha The elastic-net mixing parameter that controls the relative
#' contribution from the lasso (l1) and the ridge (l2) penalty. The penalty is
#' defined as \deqn{ \alpha||\beta||_1 + (1-\alpha)/2||\beta||_2^2.}
#' \code{alpha = 1} is the lasso penalty and \code{alpha} in between `0`
#' (`1e-6`) and `1` is the elastic-net penalty. Default is `0.5`.
#' @param lambda.min The smallest value for lambda, **as a fraction of
#' lambda.max**. Default is `.001` if the number of observations is larger than
#' the number of covariates and `.01` otherwise.
#' @param nlambda The number of lambda values. Default is `100`.
#' @param eps Convergence threshold for inner coordinate descent.
#' The algorithm iterates until the maximum change in the objective after any
#' coefficient update is less than `eps` times the null deviance.
#' Default value is `1e-7`.
#' @param max.iter Maximum number of iterations. Default is `1000`.
#' @param dfmax Upper bound for the number of nonzero coefficients. Default is
#' `20e3` because, for large data sets, computational burden may be
#' heavy for models with a large number of nonzero coefficients.
#' @param warn Return warning messages for failures to converge and model
#' saturation? Default is `TRUE`.
#'
#' @return A named list with following variables:
#'   \item{intercept}{A vector of intercepts, corresponding to each lambda.}
#'   \item{beta}{The fitted matrix of coefficients, store in sparse matrix
#'     representation. The number of rows is equal to the number of
#'     coefficients, and the number of columns is equal to `nlambda`.}
#'   \item{iter}{A vector of length `nlambda` containing the number of
#'     iterations until convergence at each value of `lambda`.}
#'   \item{lambda}{The sequence of regularization parameter values in the path.}
#'   \item{family}{Either `"gaussian"` or `"binomial"` depending on the
#'     function used.}
#'   \item{alpha}{Input parameter.}
#'   \item{loss}{A vector containing either the residual sum of squares
#'     (for linear models) or negative log-likelihood (for logistic models)
#'     of the fitted model at each value of `lambda`.}
#'   \item{n}{The number of observations used in the model fitting. It's equal
#'     to `length(row.idx)`.}
#'   \item{p}{The number of dimensions (including covariables,
#'     but not the intercept).}
#'   \item{center}{The sample mean vector of the variables, i.e., column mean
#'     of the sub-matrix of `X` used for model fitting.}
#'   \item{scale}{The sample standard deviation of the variables, i.e.,
#'     column standard deviation of the sub-matrix of `X` used for model
#'     fitting.}
#'   \item{y}{The response vector used in the model fitting. Depending on
#'     `row.idx`, it could be a subset of the raw input of the response vector
#'     y.}
#'   \item{col.idx}{The indices of features that have 'scale' value greater
#'     than `1e-6`. Features with 'scale' less than 1e-6 are removed from
#'     model fitting.}
#'
#' @import Matrix
#' @include AUC.R
#' @keywords internal
COPY_biglasso_main <- function(X, y.train, ind.train, ind.col, covar.train,
                               family = c("gaussian", "binomial"),
                               alpha = 0.5,
                               K = 10,
                               ind.sets = sample(rep_len(1:K, n)),
                               nlambda = 200,
                               lambda.min = `if`(n > p, .0001, .001),
                               nlam.min = 50,
                               n.abort = 10,
                               eps = 1e-7,
                               max.iter = 1000,
                               dfmax = 20e3,
                               warn = TRUE,
                               ncores = 1) {

  family <- match.arg(family)

  n <- length(ind.train)
  if (is.null(covar.train)) covar.train <- matrix(0, n, 0)
  assert_lengths(y.train, ind.train, rows_along(covar.train), ind.sets)
  p <- length(ind.col) + ncol(covar.train)

  if (alpha > 1 || alpha < 1e-4) stop("alpha must be between 1e-4 and 1.")

  if (nlambda < 2) stop("nlambda must be at least 2")

  if (any(is.na(y.train)))
    stop(paste("Missing data (NA's) detected. Take actions",
               "(e.g., removing cases, removing features, imputation)",
               "to eliminate missing data before fitting the model."))

  if (class(y.train) != "numeric")
    tryCatch(y.train <- as.numeric(y.train), error = function(e)
      stop("y.train must numeric or able to be coerced to numeric"))

  if (family == "binomial") y.train <- transform_levels(y.train)

  # Get summaries
  ## Parallelize over columns
  list_summaries <-
    big_apply(X, a.FUN = function(X, ind, y.train, ind.train, ind.sets, K) {
      list(
        summaries(X, y.train, ind.train, ind, ind.sets = ind.sets, K = K)
      )
    }, a.combine = 'c', ncores = ncores, ind = ind.col, y.train = y.train,
    ind.train = ind.train, ind.sets = ind.sets, K = K)
  ## Get also for covariables
  summaries.covar <-
    summaries(X, y.train, ind.train, integer(0), covar.train, ind.sets, K)

  ## fit models
  if (ncores == 1) {
    registerDoSEQ()
  } else {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }
  cross.res <- foreach(ic = 1:K) %dopar% {

    print(ic)

    in.val <- (ind.sets == ic)

    ## Merge summaries
    keep   <- do.call('c', lapply(list_summaries, function(x) x[["keep"]]))
    center <- do.call('c', lapply(list_summaries, function(x) x[["center"]][ic, ]))[keep]
    scale  <- do.call('c', lapply(list_summaries, function(x) x[["scale"]][ic, ]))[keep]
    resid  <- do.call('c', lapply(list_summaries, function(x) x[["resid"]][ic, ]))[keep]
    keep.covar <- summaries.covar$keep
    center <- c(center, summaries.covar[["center"]][ic, keep.covar])
    scale  <- c(scale,  summaries.covar[["scale"]][ic, keep.covar])
    resid  <- c(resid,  summaries.covar[["resid"]][ic, keep.covar])
    ## Compute lambdas of the path
    lambda.max <- max(abs(resid)) / alpha
    lambda <- exp(
      seq(log(lambda.max), log(lambda.max * lambda.min), length.out = nlambda))

    mod <- COPY_biglasso_part(
      X, y.train = y.train[!in.val],
      ind.train = ind.train[!in.val],
      ind.col = ind.col[keep],
      covar.train = covar.train[!in.val, keep.covar, drop = FALSE],
      family, lambda, center, scale, resid, alpha,
      eps, max.iter, dfmax, warn,
      ind.val = ind.train[in.val],
      covar.val = covar.train[in.val, keep.covar, drop = FALSE],
      y.val = y.train[in.val],
      n.abort, nlam.min
    )

    # scores <- predict(mod, X, ind.row = ind.train[in.val],
    #                   covar.row = covar.train[in.val, ])
    #
    # list(betas = mod$beta, scores = scores)
  }
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
#' @inheritParams bigstatsr-package
#' @inheritDotParams COPY_biglasso_part -X -y.train -ind.train -covar.train -family
#'
#' @inherit COPY_biglasso_part return
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
#' \url{http://dx.doi.org/10.1111/j.1467-9868.2011.01004.x}.
#'
#' Zeng, Y., and Breheny, P. (2016). The biglasso Package: A Memory- and
#' Computation-Efficient Solver for Lasso Model Fitting with Big Data in R.
#' arXiv preprint arXiv:1701.05936. \url{https://arxiv.org/abs/1701.05936}.
#'
#' @export
big_spLinReg <- function(X, y.train,
                         ind.train = rows_along(X),
                         ind.col = cols_along(X),
                         covar.train = NULL, ...) {

  check_args()

  COPY_biglasso_main(X, y.train, ind.train, ind.col, covar.train,
                     family = "gaussian", ...)
}

################################################################################

#' Sparse logistic regression
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams COPY_biglasso_part -X -y.train -ind.train -covar.train -family
#'
#' @inherit big_spLinReg return description details seealso references
#'
#' @example examples/example-spLogReg.R
#'
#' @export
big_spLogReg <- function(X, y01.train,
                         ind.train = rows_along(X),
                         ind.col = cols_along(X),
                         covar.train = NULL, ...) {

  check_args()

  COPY_biglasso_main(X, y01.train, ind.train, ind.col, covar.train,
                     family = "binomial", ...)
}

################################################################################
