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
#' __This is a modified version of package of
#' [package biglasso](https://github.com/YaohuiZeng/biglasso)__.
#'
#'
#' @param row.idx The integer vector of row indices of \code{X} that used for
#' fitting the model. \code{1:nrow(X)} by default.
#' @param penalty The penalty to be applied to the model. Either "lasso" (the
#' default), "ridge", or "enet" (elastic net).
#' @param family Either "gaussian" or "binomial", depending on the response.
#' @param alpha The elastic-net mixing parameter that controls the relative
#' contribution from the lasso (l1) and the ridge (l2) penalty. The penalty is
#' defined as \deqn{ \alpha||\beta||_1 + (1-\alpha)/2||\beta||_2^2.}
#' \code{alpha=1} is the lasso penalty, \code{alpha=0} the ridge penalty,
#' \code{alpha} in between 0 and 1 is the elastic-net ("enet") penalty.
#' @param lambda.min The smallest value for lambda, as a fraction of
#' lambda.max.  Default is .001 if the number of observations is larger than
#' the number of covariates and .01 otherwise.
#' @param nlambda The number of lambda values.  Default is 100.
#' @param lambda.log.scale Whether compute the grid values of lambda on log
#' scale (default) or linear scale.
#' @param lambda A user-specified sequence of lambda values.  By default, a
#' sequence of values of length \code{nlambda} is computed, equally spaced on
#' the log scale.
#' @param eps Convergence threshold for inner coordinate descent.  The
#' algorithm iterates until the maximum change in the objective after any
#' coefficient update is less than \code{eps} times the null deviance. Default
#' value is \code{1e-7}.
#' @param max.iter Maximum number of iterations. Default is 1000.
#' @param dfmax Upper bound for the number of nonzero coefficients.  Default is
#' no upper bound.  However, for large data sets, computational burden may be
#' heavy for models with a large number of nonzero coefficients.
#' @param penalty.factor A multiplicative factor for the penalty applied to
#' each coefficient.  If supplied, \code{penalty.factor} must be a numeric
#' vector of length equal to the number of columns of \code{X}.  The purpose of
#' \code{penalty.factor} is to apply differential penalization if some
#' coefficients are thought to be more likely than others to be in the model.
#' Current package doesn't allow unpenalized coefficients. That
#' is\code{penalty.factor} cannot be 0.
#' @param warn Return warning messages for failures to converge and model
#' saturation?  Default is TRUE.
#' @param output.time Whether to print out the start and end time of the model
#' fitting.
#' @param verbose Whether to output the timing of each lambda iteration.
#' Default is FALSE.
#' @return An object with S3 class \code{"biglasso"} with following variables.
#' \item{beta}{The fitted matrix of coefficients, store in sparse matrix
#' representation. The number of rows is equal to the number of coefficients,
#' whereas the number of columns is equal to \code{nlambda}.} \item{iter}{A
#' vector of length \code{nlambda} containing the number of iterations until
#' convergence at each value of \code{lambda}.} \item{lambda}{The sequence of
#' regularization parameter values in the path.} \item{penalty}{Same as above.}
#' \item{family}{Same as above.} \item{alpha}{Same as above.} \item{loss}{A
#' vector containing either the residual sum of squares (\code{for "gaussian"})
#' or negative log-likelihood (for \code{"binomial"}) of the fitted model at
#' each value of \code{lambda}.} \item{penalty.factor}{Same as above.}
#' \item{n}{The number of observations used in the model fitting. It's equal to
#' \code{length(row.idx)}.} \item{center}{The sample mean vector of the
#' variables, i.e., column mean of the sub-matrix of \code{X} used for model
#' fitting.} \item{scale}{The sample standard deviation of the variables, i.e.,
#' column standard deviation of the sub-matrix of \code{X} used for model
#' fitting.} \item{y}{The response vector used in the model fitting. Depending
#' on \code{row.idx}, it could be a subset of the raw input of the response
#' vector y.} \item{screen}{Same as above.} \item{col.idx}{The indices of
#' features that have 'scale' value greater than 1e-6. Features with 'scale'
#' less than 1e-6 are removed from model fitting.} \item{rejections}{The number
#' of features rejected at each value of \code{lambda}.}
#'
COPY_biglasso <- function(X, y.train, ind.train = 1:nrow(X), covar.train = NULL,
                          family = c("gaussian", "binomial"),
                          alpha = 1,
                          lambda.min = `if`(nrow(X) > ncol(X), .001, .01),
                          nlambda = 100, lambda.log.scale = TRUE,
                          lambda, eps = 1e-7, max.iter = 1000,
                          dfmax = ncol(X)+1,
                          penalty.factor = rep(1, ncol(X)),
                          warn = TRUE,
                          verbose = FALSE) {

  family <- match.arg(family)
  penalty <- match.arg(penalty)
  lambda.min <- max(lambda.min, 1e-6)
  if (is.null(covar.train)) covar.train <- matrix(0, 0, 0)
  penalty.factor <- c(penalty.factor, rep(1, ncol(covar.train)))

  if (alpha == 1) {
    penalty <- "lasso"
  } else if (alpha < 1 && alpha > 1e-6) {
    penalty <- "enet"
  } else {
    stop("alpha must be between 1e-6 and 1 for elastic net penalty.")
  }

  if (nlambda < 2) stop("nlambda must be at least 2")

  if (any(is.na(y.train))) stop("Missing data (NA's) detected.  Take actions (e.g., removing cases, removing features, imputation) to eliminate missing data before fitting the model.")

  if (class(y.train) != "numeric") {
    tmp <- try(y.train <- as.numeric(y.train), silent=TRUE)
    if (class(tmp)[1] == "try-error") stop("y.train must numeric or able to be coerced to numeric")
  }

  if (family == 'binomial') {
    if (length(table(y.train)) > 2) {
      stop("Attemping to use family='binomial' with non-binary data")
    }
    if (!identical(sort(unique(y.train)), 0:1)) {
      y.train <- as.numeric(y.train == max(y.train))
    }
    n.pos <- sum(y.train) # number of 1's
    ylab <- ifelse(y.train == 0, -1, 1) # response label vector of {-1, 1}
  }

  if (family=="gaussian") {
    yy <- y.train - mean(y.train)
  } else {
    yy <- y.train
  }

  p <- length(penalty.factor)

  n <- length(ind.train) ## subset of X. idx: indices of rows.
  if (missing(lambda)) {
    user.lambda <- FALSE
    lambda <- rep(0.0, nlambda);
  } else {
    nlambda <- length(lambda)
    user.lambda <- TRUE
  }

  ## fit model
  if (verbose) cat("\nStart biglasso: ", format(Sys.time()), '\n')

  if (family == 'gaussian') {
    res <- COPY_cdfit_gaussian_hsr(X@address, yy, ind.train-1, covar.train,
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

  } else if (family == 'binomial') {
    res <- COPY_cdfit_binomial_hsr(X@address, yy, ind.train-1, covar.train,
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
  if (verbose) cat("\nEnd biglasso: ", format(Sys.time()), '\n')

  # p.keep <- length(col.idx)
  col.idx <- col.idx + 1 # indices (in R) for which variables have scale > 1e-6

  ## Eliminate saturated lambda values, if any
  ind <- !is.na(iter)
  a <- a[ind]
  b <- b[, ind, drop = FALSE]
  iter <- iter[ind]
  lambda <- lambda[ind]
  loss <- loss[ind]

  if (warn & any(iter==max.iter)) warning("Algorithm failed to converge for some values of lambda")

  ## Unstandardize coefficients:
  beta <- Matrix(0, nrow = (p+1), ncol = length(lambda), sparse = T)
  bb <- b / scale[col.idx]
  beta[col.idx+1, ] <- bb
  beta[1,] <- a - crossprod(center[col.idx], bb)

  ## Names
  varnames <- if (is.null(colnames(X))) paste("V", 1:p, sep="") else colnames(X)
  varnames <- c("(Intercept)", varnames)
  dimnames(beta) <- list(varnames, round(lambda, digits = 4))

  ## Output
  return.val <- list(
    beta = beta,
    iter = iter,
    lambda = lambda,
    penalty = penalty,
    family = family,
    alpha = alpha,
    loss = loss,
    penalty.factor = penalty.factor,
    n = n,
    center = center,
    scale = scale,
    y = yy,
    col.idx = col.idx,
    rejections = rejections
  )

  val <- structure(return.val, class = c("biglasso", 'ncvreg'))
  val
}

################################################################################
