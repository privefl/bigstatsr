################################################################################

#' Sparse logistic regression
#'
#' Fit lasso penalized logistic regression path for a `big.matrix`.
#' Covariates can be added to correct for confounders.
#' __This is a modified version of package of
#' [package biglasso](https://github.com/YaohuiZeng/biglasso)__.
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams COPY_biglasso -X -y.train -ind.train -covar.train -family
#'
#' @inherit big_spRegLin return seealso references
#'
#' @example
#'
#' @export
big_spRegLog <- function(X, y01.train, ind.train = seq(nrow(X)),
                         covar.train = NULL, ...) {
  check_biglasso()

  COPY_biglasso(X, y01.train, ind.train, covar.train, family = "binomial", ...)
}

################################################################################

#' Title
#'
#' @param X
#' @param y01.train
#' @param ind.train
#' @param covar.train
#' @param K
#' @param ncores
#' @param ...
#'
#' @return
#' @export
#' @import foreach
#'
#' @examples
big_spRegLogCV2 <- function(X, y01.train, ind.train = seq(nrow(X)),
                            covar.train = NULL, K = 10, ncores = 1, ...) {
  check_X(X, ncores = ncores)
  check_biglasso()

  X.desc <- describe(X)
  n <- length(ind.train)
  ind <- sample(rep_len(1:K, n))

  if (is.seq <- (ncores == 1)) {
    registerDoSEQ()
  } else {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }
  foreach(ic = 1:K, .packages = "biglasso") %dopar% {
    X2 <- attach.big.matrix(X.desc)

    i.test <- which(ind == ic)
    i.train <- setdiff(1:n, i.test)

    mod <- big_spRegLog(X2, y01.train[i.train], ind.train[i.train],
                        covar.train[i.train, ], ncores = 1, ...)
    tmp.ind.test <- ind.train[i.test]

    betas <- mod$beta
    ind.col <- sort(unique(betas@i))[-1] # without intercept
    scores <- as.matrix(X2[tmp.ind.test, ind.col] %*% betas[ind.col + 1, ])
    rownames(scores) <- tmp.ind.test

    list(betas = betas, scores = sweep(scores, 2, betas[1, ], '+'))
  }
}

################################################################################
