################################################################################

########################################################
### This is a modified version from package biglasso ###
###      https://github.com/YaohuiZeng/biglasso      ###
########################################################

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
#' (`1e-6`) and `1` is the elastic-net penalty.
#' @param lambda.min The smallest value for lambda, as a fraction of
#' lambda.max. Default is `.001` if the number of observations is larger than
#' the number of covariates and `.01` otherwise.
#' @param nlambda The number of lambda values. Default is `100`.
#' @param lambda.log.scale Whether compute the grid values of lambda on log
#' scale (default) or linear scale.
#' @param lambda A user-specified sequence of lambda values. By default, a
#' sequence of values of length `nlambda` is computed, equally spaced on
#' the log scale.
#' @param eps Convergence threshold for inner coordinate descent.
#' The algorithm iterates until the maximum change in the objective after any
#' coefficient update is less than `eps` times the null deviance.
#' Default value is `1e-7`.
#' @param max.iter Maximum number of iterations. Default is `1000`.
#' @param dfmax Upper bound for the number of nonzero coefficients. Default is
#' no upper bound. However, for large data sets, computational burden may be
#' heavy for models with a large number of nonzero coefficients.
#' @param penalty.factor A multiplicative factor for the penalty applied to
#' each coefficient. If supplied, `penalty.factor` must be a numeric
#' vector of length equal to sum of the number of columns of `X` and the
#' number of covariables (intercept excluded). The purpose of `penalty.factor`
#' is to apply differential penalization if some coefficients are thought to be
#' more likely than others to be in the model. Current package doesn't allow
#' unpenalized coefficients. That is `penalty.factor` cannot be 0.
#' @param warn Return warning messages for failures to converge and model
#' saturation? Default is `TRUE`.
#' @param verbose Whether to print out the start, the timing of each lambda
#' iteration and the end. Default is `FALSE`.
#'
#' @return A named list with following variables:
#'   \item{intercept}{A vector of intercepts, corresponding to each lambda.}
#'   \item{beta}{The fitted matrix of coefficients, store in sparse matrix
#'     representation. The number of rows is equal to the number of
#'     coefficients, and the number of columns is equal to `nlambda`.}
#'   \item{iter}{A vector of length `nlambda` containing the number of
#'     iterations until convergence at each value of `lambda`.}
#'   \item{lambda}{The sequence of regularization parameter values in the path.}
#'   \item{penalty}{Penalty used. See the input parameter `alpha`.}
#'   \item{family}{Either `"gaussian"` or `"binomial"` depending on the
#'     function used.}
#'   \item{alpha}{Input parameter.}
#'   \item{loss}{A vector containing either the residual sum of squares
#'     (for linear models) or negative log-likelihood (for logistic models)
#'     of the fitted model at each value of `lambda`.}
#'   \item{penalty.factor}{Input parameter.}
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
#'   \item{rejections}{The number of features rejected at each value of
#'   `lambda`.}
#'
#' @import Matrix
#' @keywords internal
COPY_biglasso <- function(X, y.train, ind.train, covar.train,
                          family = c("gaussian", "binomial"),
                          alpha = 1,
                          lambda.min = `if`(nrow(X) > ncol(X), .001, .01),
                          nlambda = 100, lambda.log.scale = TRUE,
                          lambda, eps = 1e-7, max.iter = 1000,
                          dfmax = p + 1,
                          penalty.factor = NULL,
                          warn = TRUE,
                          verbose = FALSE) {

  family <- match.arg(family)
  lambda.min <- max(lambda.min, 1e-6)

  n <- length(ind.train) ## subset of X. idx: indices of rows.
  if (is.null(covar.train)) covar.train <- matrix(0, n, 0)
  assert_lengths(y.train, ind.train, rows_along(covar.train))

  p <- ncol(X) + ncol(covar.train)
  if (is.null(penalty.factor)) penalty.factor <- rep(1, p)
  if (p != length(penalty.factor))
    stop("'penalty.factor' has an incompatible length.")

  if (alpha == 1) {
    penalty <- "lasso"
  } else if (alpha < 1 && alpha > 1e-6) {
    penalty <- "enet"
  } else {
    stop("alpha must be between 1e-6 and 1 for elastic net penalty.")
  }

  if (nlambda < 2) stop("nlambda must be at least 2")

  if (any(is.na(y.train)))
    stop(paste("Missing data (NA's) detected. Take actions",
               "(e.g., removing cases, removing features, imputation)",
               "to eliminate missing data before fitting the model."))

  if (class(y.train) != "numeric")
    tryCatch(y.train <- as.numeric(y.train), error = function(e)
      stop("y.train must numeric or able to be coerced to numeric"))

  if (family == "binomial") {
    yy <- transform_levels(y.train)
  } else if (family == "gaussian") {
    yy <- y.train - mean(y.train)
  } else {
    stop("Current version only supports Gaussian or Binominal response!")
  }

  if (missing(lambda)) {
    user.lambda <- FALSE
    lambda <- rep(0, nlambda);
  } else {
    nlambda <- length(lambda)
    user.lambda <- TRUE
  }

  ## fit model
  if (verbose) printf("\nStart biglasso: %s\n", format(Sys.time()))

  if (family == "gaussian") {
    res <- COPY_cdfit_gaussian_hsr(X, yy, ind.train - 1, covar.train,
                                   lambda, nlambda, lambda.log.scale,
                                   lambda.min, alpha,
                                   user.lambda | any(penalty.factor == 0),
                                   eps, max.iter, penalty.factor,
                                   dfmax, verbose)

    a <- rep(mean(y.train), nlambda)
    b <- Matrix(res[[1]], sparse = TRUE)
    center <- res[[2]]
    scale <- res[[3]]
    lambda <- res[[4]]
    loss <- res[[5]]
    iter <- res[[6]]
    rejections <- res[[7]]
    col.idx <- res[[8]]

  } else if (family == "binomial") {
    res <- COPY_cdfit_binomial_hsr(X, yy, ind.train - 1, covar.train,
                                   lambda, nlambda, lambda.log.scale,
                                   lambda.min, alpha,
                                   user.lambda | any(penalty.factor == 0),
                                   eps, max.iter, penalty.factor,
                                   dfmax, warn, verbose)


    a <- res[[1]]
    b <- Matrix(res[[2]], sparse = TRUE)
    center <- res[[3]]
    scale <- res[[4]]
    lambda <- res[[5]]
    loss <- res[[6]]
    iter <- res[[7]]
    rejections <- res[[8]]
    col.idx <- res[[9]]

  } else {
    stop("Current version only supports Gaussian or Binominal response!")
  }
  if (verbose) printf("\nEnd biglasso: %s\n", format(Sys.time()))

  # p.keep <- length(col.idx)
  col.idx <- col.idx + 1 # indices (in R) for which variables have scale > 1e-6

  ## Eliminate saturated lambda values, if any
  ind <- !is.na(iter)
  a <- a[ind]
  b <- b[, ind, drop = FALSE]
  iter <- iter[ind]
  lambda <- lambda[ind]
  loss <- loss[ind]

  if (warn & any(iter == max.iter))
    warning("Algorithm failed to converge for some values of lambda")

  ## Unstandardize coefficients:
  beta <- Matrix(0, nrow = p, ncol = length(lambda), sparse = TRUE)
  bb <- b / scale[col.idx]
  beta[col.idx, ] <- bb
  aa <- a - as.numeric(crossprod(center[col.idx], bb))

  ## Names
  names(aa) <- colnames(beta) <- round(lambda, digits = 4)

  ## Output
  structure(list(
    intercept = aa,
    beta = beta,
    iter = iter,
    lambda = lambda,
    penalty = penalty,
    family = family,
    alpha = alpha,
    loss = loss,
    penalty.factor = penalty.factor,
    n = n,
    p = p,
    center = center,
    scale = scale,
    y = yy,
    col.idx = col.idx,
    rejections = rejections
  ), class = "big_sp")
}

################################################################################

#' Sparse linear regression
#'
#' Fit lasso penalized linear regression path for a `big.matrix`.
#' Covariates can be added to correct for confounders.
#'
#' __This is a modified version of one function of
#' [package biglasso](https://github.com/YaohuiZeng/biglasso)__.
#' It adds the possibility to train models with covariables and use many
#' types of `big.matrix` (not only `double` ones).
#' Yet, it only corresponds to `screen = "SSR"` (Sequential Strong Rules).
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams COPY_biglasso -X -y.train -ind.train -covar.train -family
#'
#' @inherit COPY_biglasso return
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
big_spLinReg <- function(X., y.train, ind.train = rows_along(X.),
                         covar.train = NULL, ...) {

  check_args()

  X <- attach.BM(X.)
  COPY_biglasso(X, y.train, ind.train, covar.train, family = "gaussian", ...)
}

################################################################################

#' Sparse logistic regression
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams COPY_biglasso -X -y.train -ind.train -covar.train -family
#'
#' @inherit big_spLinReg return description details seealso references
#'
#' @example examples/example-spLogReg.R
#'
#' @export
big_spLogReg <- function(X., y01.train, ind.train = rows_along(X.),
                         covar.train = NULL, ...) {

  check_args()

  X <- attach.BM(X.)
  COPY_biglasso(X, y01.train, ind.train, covar.train, family = "binomial", ...)
}

################################################################################
