################################################################################

get_beta <- function(betas, method) {
  if (method == "geometric-median") {
    # Weiszfeld's algorithm
    # probabilistically impossible to not converge in this situation?
    b.old <- rowMeans(betas)

    repeat {
      norm <- sqrt(colSums((betas - b.old)^2))
      b.new <- rowSums(sweep(betas, 2, norm, '/')) / sum(1 / norm)
      dif <- max(abs(b.new - b.old))
      if (dif < 1e-10) break
      b.old <- b.new
    }

    b.new
  } else if (method == "mean-wise") {
    rowMeans(betas)
  } else if (method == "median-wise") {
    apply(betas, 1, median)
  } else {
    stop("This method is not implemented.")
  }
}

################################################################################

#' Cross-Model Selection and Averaging.
#'
#' 1. This function separates the training set in `K` folds.
#' 2. __In turn__,
#'     - each fold is considered as an inner validation set and the others
#'       (K - 1) folds form an inner training set,
#'     - the model is trained on the inner training set and the corresponding
#'       predictions (scores) for the inner validation set are computed,
#'     - the vector of coefficients corresponding to the vector of scores which
#'       maximizes `feval` is chosen.
#' 3. The `K` resulting vectors of coefficients are then combined into one
#' vector (see the `method` parameter).
#'
#' @inheritParams bigstatsr-package
#' @param FUN Function that computes a linear scores for different
#' values along a regularization path. For now, this is relevant to
#' [big_spLinReg], [big_spLogReg] and [big_spSVM].
#' The corresponding `<FUN>.predict` function should exists.
#' @param K Number of folds that are used.
#' @param ... Extra parameters to be passed to FUN.
#' @param feval Custimized evaluation function. You should always aim at
#' maximizing this function. If `feval` is like a loss function (which you
#' want to minimize), you should use its opposite instead.
#' Its only two arguments should be `pred` and `target`
#' (e.g. `function(pred, target) aucSample(pred, target, nsim = 1e5)`).
#' @param method Method for combining vectors of coefficients. The default uses
#' the [geometric median](https://en.wikipedia.org/wiki/Geometric_median).
#'
#' @return A vector of resulting coefficients (intercept excluded), which
#' corresponds to the training set. You might want to recompute an optimal
#' intercept if needed.
#' @export
#' @import foreach
#'
big_CMSA <- function(FUN, X, y, ind.train = seq(nrow(X)),
                     covar = NULL, K = 10, ncores = 1,
                     method = c("geometric-median", "mean-wise", "median-wise"),
                     block.size = 1000,
                     ...) {
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
  cross.res <- foreach(ic = 1:K) %dopar% {
    X2 <- attach.big.matrix(X.desc)

    in.val <- (ind == ic)
    tmp.ind.test <- ind.train[in.val]
    tmp.ind.train <- ind.train[!in.val]

    mod <- FUN(X2, y[!in.val], ind.train[!in.val], covar[!in.val, ], ...)

    scores <- predict.big_sp(mod, X, ind.row = ind.train[in.val],
                             covar.row = covar[in.val, ],
                             block.size = block.size,
                             ncores2 = max(1, ncores / 2))

    list(betas = mod$beta, scores = scores)
  }

  # select best coefficients for each fold
  betas <- sapply(cross.res, function(x) {
    tmp <- x$scores
    ind <- as.numeric(rownames(tmp))
    seval <- apply(tmp, 2, feval, target = pheno[ind])
    x$betas[, which.max(seval)]
  })
  # average these coefficients
  get_beta(betas, method)
}

################################################################################
