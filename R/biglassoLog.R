################################################################################

#' Sparse logistic regression
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams COPY_biglasso -X -y.train -ind.train -covar.train -family
#'
#' @inherit big_spRegLin return description details seealso references
#'
#' @example examples/example-spRegLog.R
#'
#' @export
big_spRegLog <- function(X, y01.train, ind.train = seq(nrow(X)),
                         covar.train = NULL, ...) {
  COPY_biglasso(X, y01.train, ind.train, covar.train, family = "binomial", ...)
}

################################################################################

#' Title
#'
#' @inheritParams bigstatsr-package
#' @param K
#' @param ncores
#' @param ...
#'
#' @return
#' @export
#' @import foreach
#'
big_spRegLogCV2 <- function(X, y01.train, ind.train = seq(nrow(X)),
                            covar.train = NULL, K = 10, ncores = 1, ...) {
  check_X(X, ncores = ncores)

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
  foreach(ic = 1:K) %dopar% {
    X2 <- attach.big.matrix(X.desc)

    i.test <- which(ind == ic)
    i.train <- setdiff(1:n, i.test)

    mod <- big_spRegLog(X2, y01.train[i.train], ind.train[i.train],
                        covar.train[i.train, ], ...)
    tmp.ind.test <- ind.train[i.test]

    betas <- mod$beta
    ind.col <- sort(unique(betas@i))
    scores <- as.matrix(X2[tmp.ind.test, ind.col] %*% betas[ind.col, ])
    rownames(scores) <- tmp.ind.test

    list(betas = betas, scores = sweep(scores, 2, mod$intercept, '+'))
  }
}

################################################################################
