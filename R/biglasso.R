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

#' Train one model
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
#'   \item{loss.val}{A vector containing the loss for the corresponding
#'     validation set.}
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
#'
#' @keywords internal
#'
COPY_biglasso_part <- function(X, y.train, ind.train, ind.col, covar.train,
                               family, lambda, center, scale, resid, alpha,
                               eps, max.iter, dfmax, warn,
                               ind.val, covar.val, y.val, n.abort, nlam.min) {

  assert_lengths(y.train, ind.train, rows_along(covar.train))
  assert_lengths(y.val, ind.val, rows_along(covar.val))
  assert_lengths(c(ind.col, cols_along(covar.train)), center, scale, resid)
  stopifnot(length(intersect(ind.train, ind.val)) == 0)

  ## fit model
  if (family == "gaussian") {

    y.train.mean <- mean(y.train)

    res <- COPY_cdfit_gaussian_hsr(
      X, y.train - y.train.mean, ind.train, ind.col, covar.train,
      lambda, center, scale, resid, alpha, eps, max.iter, dfmax, warn,
      ind.val, covar.val, y.val - y.train.mean, n.abort, nlam.min)

    b <- Matrix(res[[1]], sparse = TRUE)
    a <- rep(y.train.mean, ncol(b))
    loss <- res[[2]]
    iter <- res[[3]]
    loss.val <- res[[4]]

  } else if (family == "binomial") {

    res <- COPY_cdfit_binomial_hsr(
      X, y.train, ind.train, ind.col, covar.train,
      lambda, center, scale, resid, alpha, eps, max.iter, dfmax, warn,
      ind.val, covar.val, y.val, n.abort, nlam.min)

    a <- res[[1]]
    b <- Matrix(res[[2]], sparse = TRUE)
    loss <- res[[3]]
    iter <- res[[4]]
    loss.val <- res[[5]]

  } else {
    stop("Current version only supports Gaussian or Binominal response!")
  }

  ## Eliminate saturated lambda values, if any
  ind <- !is.na(iter)
  a <- a[ind]
  b <- b[, ind, drop = FALSE]
  iter <- iter[ind]
  lambda <- lambda[ind]
  loss <- loss[ind] / length(ind.train)
  loss.val <- loss.val[ind] / length(ind.val)

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
    loss.val = loss.val,
    ind.train = ind.train,
    ind.col = ind.col
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
#' (`1e-4`) and `1` is the elastic-net penalty. Default is `0.5`. **You can
#' pass multiple values, and only one will be used (optimized by grid-search).**
#' @param lambda.min The smallest value for lambda, **as a fraction of
#' lambda.max**. Default is `.0001` if the number of observations is larger than
#' the number of variables and `.001` otherwise.
#' @param nlambda The number of lambda values. Default is `200`.
#' @param eps Convergence threshold for inner coordinate descent.
#' The algorithm iterates until the maximum change in the objective after any
#' coefficient update is less than `eps` times the null deviance.
#' Default value is `1e-7`.
#' @param max.iter Maximum number of iterations. Default is `1000`.
#' @param dfmax Upper bound for the number of nonzero coefficients. Default is
#' `20e3` because, for large data sets, computational burden may be
#' heavy for models with a large number of nonzero coefficients.
#' @param warn Return warning messages for failures to converge and model
#' saturation? Default is `FALSE`.
#' @param K Number of sets used in the Cross-Model Selection and Averaging
#'   (CMSA) procedure. Default is `10`.
#' @param ind.sets Integer vectors of values between `1` and `K` specifying
#'   which set each index of the training set is in. Default randomly assigns
#'   these values.
#' @param return.all Whether to return coefficients for all alpha and lambda
#'   values. Default is `FALSE` and returns only coefficients which maximize
#'   prediction on the validation sets.
#' @param nlam.min Minimum number of lambda values to investigate. Default is `50`.
#' @param n.abort Number of lambda values for which prediction on the validation
#'   set must decrease before stopping. Default is `10`.
#'
#' @keywords internal
#'
COPY_biglasso_main <- function(X, y.train, ind.train, ind.col, covar.train,
                               family = c("gaussian", "binomial"),
                               alphas = 0.5,
                               K = 10,
                               ind.sets = sample(rep_len(1:K, n)),
                               nlambda = 200,
                               lambda.min = `if`(n > p, .0001, .001),
                               nlam.min = 50,
                               n.abort = 10,
                               eps = 1e-7,
                               max.iter = 1000,
                               dfmax = 20e3,
                               warn = FALSE,
                               return.all = FALSE,
                               ncores = 1) {

  family <- match.arg(family)

  n <- length(ind.train)
  if (is.null(covar.train)) covar.train <- matrix(0, n, 0)
  assert_lengths(y.train, ind.train, rows_along(covar.train), ind.sets)
  p <- length(ind.col) + ncol(covar.train)

  if (any(alphas < 1e-4 | alphas > 1)) stop("alpha must be between 1e-4 and 1.")

  if (nlambda < 2) stop("nlambda must be at least 2")

  if (any(is.na(y.train)))
    stop(paste("Missing data (NA's) detected. Take actions",
               "(e.g., removing cases, removing features, imputation)",
               "to eliminate missing data before fitting the model."))

  if (class(y.train) != "numeric")
    tryCatch(y.train <- as.numeric(y.train), error = function(e)
      stop("y.train must numeric or able to be coerced to numeric"))

  if (family == "binomial") {
    y.train <- transform_levels(y.train)
  } else {
    assert_multiple(y.train)
  }

  # Get summaries
  ## Get also for covariables
  summaries.covar <-
    summaries(X, y.train, ind.train, integer(0), covar.train, ind.sets, K)
  if (!all(summaries.covar$keep))
    stop("Please use covariables with some variation.")

  ## Parallelize over columns
  list_summaries <-
    big_apply(X, a.FUN = function(X, ind, y.train, ind.train, ind.sets, K) {
      list(
        summaries(X, y.train, ind.train, ind, ind.sets = ind.sets, K = K)
      )
    }, a.combine = 'c', ncores = ncores, ind = ind.col, y.train = y.train,
    ind.train = ind.train, ind.sets = ind.sets, K = K)

  keep <- do.call('c', lapply(list_summaries, function(x) x[["keep"]]))


  ## fit models
  if (ncores == 1) {
    registerDoSEQ()
  } else {
    cluster_type <- getOption("bigstatsr.cluster.type")
    cl <- parallel::makeCluster(ncores, type = cluster_type)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }

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
    lambda.max <- max(abs(resid)) / alpha
    lambda <- exp(
      seq(log(lambda.max), log(lambda.max * lambda.min), length.out = nlambda))

    COPY_biglasso_part(
      X, y.train = y.train[!in.val],
      ind.train = ind.train[!in.val],
      ind.col = ind.col[keep],
      covar.train = covar.train[!in.val, , drop = FALSE],
      family, lambda, center, scale, resid, alpha, eps, max.iter, dfmax, warn,
      ind.val = ind.train[in.val],
      covar.val = covar.train[in.val, , drop = FALSE],
      y.val = y.train[in.val],
      n.abort, nlam.min
    )
  }

  if (return.all) {
    cross.res
  } else {

    # Choose the best alpha (for the best lambdas)
    ind.min <- which.min(
      sapply(cross.res, function(l) {
        mean(sapply(l, function(x) min(x$loss.val)))
      })
    )

    structure(
      lapply(cross.res[[ind.min]], function(x) {
        best <- which.min(x$loss.val)
        ind <- seq_along(x$ind.col)
        list(
          intercept  = unname(x$intercept[best]),
          beta.X     = x$beta[ind, best],
          beta.covar = x$beta[-ind, best]
        )
      }),
      class = "big_sp_best_list",
      ind.col = ind.col[keep],
      family = family,
      alpha = alphas[ind.min]
    )
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
#' @inheritDotParams COPY_biglasso_main -X -y.train -ind.train -covar.train -family
#'
#' @return Return an object of class `big_sp_best_list` (a list of K elements),
#'   which has a method `predict` that can compute K vectors of predictions,
#'   which could be combined with e.g. `rowMeans`. See details.
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
                         ncores = 1,
                         ...) {

  check_args()

  COPY_biglasso_main(X, y.train, ind.train, ind.col, covar.train,
                     family = "gaussian", ncores = ncores, ...)
}

################################################################################

#' Sparse logistic regression
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams COPY_biglasso_main -X -y.train -ind.train -covar.train -family
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
                         ncores = 1,
                         ...) {

  check_args()

  COPY_biglasso_main(X, y01.train, ind.train, ind.col, covar.train,
                     family = "binomial", ncores = ncores, ...)
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
