################################################################################

#########################################################
### This is a modified version from package sparseSVM ###
###        https://github.com/CY-dev/sparseSVM        ###
#########################################################

#' Fit sparse linear SVM with lasso or elasti-net regularization
#'
#' Fit solution paths for sparse linear SVM regularized by lasso or elastic-net
#' over a grid of values for the regularization parameter lambda.
#'
#' The sequence of models indexed by the regularization parameter \code{lambda}
#' is fitted using a semismooth Newton coordinate descent algorithm. The
#' objective function is defined to be \deqn{\frac{1}{n} \sum hingeLoss(y_i
#' (x_i' w + b)) + \lambda\textrm{penalty}(w).}{\sum hingeLoss(y_i (x_i' w +
#' b))/n + \lambda*penalty(w).} where \deqn{hingeLoss(t) = max(0, 1-t)} and the
#' intercept \code{b} is unpenalized.
#'
#' @param alpha The elastic-net mixing parameter that controls the relative
#' contribution from the lasso and the ridge penalty. It must be a number
#' between `0` and `1`. \code{alpha=1} is the lasso penalty and \code{alpha=0}
#' the ridge penalty.
#' @param gamma The tuning parameter for huberization smoothing of hinge loss.
#' Default is `0.1`.
#' @param nlambda The number of lambda values. Default is `100`.
#' @param lambda.min The smallest value for lambda, as a fraction of
#' `lambda.max`, the data derived entry value. Default is `0.01` if the number
#' of observations is larger than the number of variables and `0.05` otherwise.
#' @param lambda A user-specified sequence of lambda values. Typical usage is
#' to leave blank and have the program automatically compute a \code{lambda}
#' sequence based on \code{nlambda} and \code{lambda.min}. Specifying
#' \code{lambda} overrides this. This argument should be used with care and
#' supplied with a decreasing sequence instead of a single value.
#' @param screen Screening rule to be applied at each \code{lambda} that
#' discards variables for speed. Either "ASR" (default), "SR" or "none". "SR"
#' stands for the strong rule, and "ASR" for the adaptive strong rule. Using
#' "ASR" typically requires fewer iterations to converge than "SR", but the
#' computing time are generally close. Note that the option "none" is used
#' mainly for debugging, which may lead to much longer computing time.
#' @param max.iter Maximum number of iterations. Default is 1000.
#' @param eps Convergence threshold. The algorithms continue until the maximum
#' change in the objective after any coefficient update is less than \code{eps}
#' times the null deviance.  Default is \code{1E-7}.
#' @param dfmax Upper bound for the number of nonzero coefficients. The
#' algorithm exits and returns a partial path if \code{dfmax} is reached.
#' Useful for very large dimensions.
#' @param penalty.factor A numeric vector of length equal to the number of
#' variables. Each component multiplies \code{lambda} to allow differential
#' penalization. Can be 0 for some variables, in which case the variable is
#' always in the model without penalization. Default is `1` for all variables.
#' @param message If set to `TRUE`, `sparseSVM` will inform the user of its
#' progress. Default is `FALSE`.
#'
#' @return A named list containing:
#'   \item{call}{The call that produced this object.}
#'   \item{intercept}{A vector of intercepts, corresponding to each lambda.}
#'   \item{beta}{The fitted matrix of coefficients. The number of rows is
#'     equal to the number of coefficients, and the number of columns is equal
#'     to \code{nlambda}.}
#'   \item{iter}{A vector of length \code{nlambda} containing the number of
#'     iterations until convergence at each value of \code{lambda}.}
#'   \item{saturated}{A logical flag for whether the number of nonzero
#'     coefficients has reached \code{dfmax}.}
#'   \item{lambda}{The sequence of regularization parameter values in the path.}
#'   \item{alpha}{Input parameter.}
#'   \item{gamma}{Input parameter.}
#'   \item{penalty.factor}{Input parameter.}
#'   \item{levels}{Levels of the output class labels.}
#'
#' @import Matrix
#' @keywords internal
COPY_sparseSVM <- function(X, y.train, ind.train, covar.train,
                           alpha = 1, gamma = 0.1, nlambda = 100,
                           lambda.min = `if`(nrow(X) > ncol(X), 0.01, 0.05),
                           lambda, screen = c("ASR", "SR", "none"),
                           max.iter = 1000, eps = 1e-5,
                           dfmax = p + 1, penalty.factor = NULL,
                           message = FALSE) {

  n <- length(y.train)
  if (is.null(covar.train)) covar.train <- matrix(0, n, 0)
  assert_lengths(y.train, ind.train, rows_along(covar.train))

  # Error checking
  screen <- match.arg(screen)
  if (alpha < 0 || alpha > 1) stop("alpha should be between 0 and 1")
  if (gamma < 0 || gamma > 1) stop("gamma should be between 0 and 1")
  if (missing(lambda) && nlambda < 2) stop("nlambda should be at least 2")

  p <- ncol(X) + ncol(covar.train)
  if (is.null(penalty.factor))
    penalty.factor <- rep(1, p)
  if (p != length(penalty.factor))
    stop("'penalty.factor' has an incompatible length.")

  if (is.factor(y.train)) {
    levels <- levels(y.train)
  } else {
    levels <- sort(unique(y.train))
  }
  if (length(levels) != 2)
    stop("currently the function only supports binary classification")

  call <- match.call()
  # convert response to +1/-1 coding
  yy <- double(n)
  yy[y.train == levels[1]] <- -1
  yy[y.train == levels[2]] <- 1
  penalty.factor <- c(0, penalty.factor) # no penalty for intercept term

  if (missing(lambda)) {
    lambda <- double(nlambda)
    user <- FALSE
  } else {
    nlambda <- length(lambda)
    user <- TRUE
  }

  # Flag for screening
  scrflag <- switch(screen, ASR = 1, SR = 2, none = 0)
  # Fitting
  fit <- COPY_sparse_svm(X, yy, ind.train-1, covar.train,
                         lambda, penalty.factor, gamma, alpha,
                         eps, lambda.min, scrflag, dfmax, max.iter,
                         user, message)
  weights <- Matrix(fit[[1]], sparse = TRUE)
  iter <- fit[[2]]
  lambda <- fit[[3]]
  saturated <- fit[[4]]
  # Eliminate saturated lambda values
  ind <- !is.na(iter)
  weights <- weights[, ind, drop = FALSE]
  iter <- iter[ind]
  lambda <- lambda[ind]

  # Names
  colnames(weights) <- round(lambda, 4)

  # Output
  structure(list(call = call,
                 intercept = weights[1, ],
                 beta = weights[-1, ],
                 iter = iter,
                 saturated = saturated,
                 lambda = lambda,
                 alpha = alpha,
                 gamma = gamma,
                 penalty.factor = penalty.factor[-1],
                 levels = levels), class = "big_sp")
}


################################################################################

#' Sparse SVM
#'
#' Fit solution paths for sparse linear SVM regularized by lasso or elastic-net
#' over a grid of values for the regularization parameter lambda.
#'
#' __This is a modified version of one function of
#' [package sparseSVM](https://github.com/CY-dev/sparseSVM)__.
#' This algorithm uses semi-newton coordinate descent. There may exist some
#' faster algorithm.
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams COPY_sparseSVM -X -y.train -ind.train -covar.train
#'
#' @inherit COPY_sparseSVM return
#'
#' @example examples/example-spSVM.R
#'
#' @seealso [LiblineaR][LiblineaR::LiblineaR] [sparseSVM][sparseSVM::sparseSVM]
#'
#' @export
big_spSVM <- function(X., y01.train, ind.train = rows_along(X.),
                      covar.train = NULL, ...) {

  check_args()

  X <- attach.BM(X.)
  COPY_sparseSVM(X, y01.train, ind.train, covar.train, ...)
}

################################################################################
