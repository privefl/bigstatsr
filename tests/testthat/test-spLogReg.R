################################################################################

context("SP_LOG_REG")

set.seed(SEED)

################################################################################

# Simulating some data
N <- 530
M <- 730
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
s <- rowSums(x[, 1:5] - 100) + 5 * rnorm(N)
y <- as.numeric(s > 0)

covar0 <- matrix(rnorm(N * 3), N)
lcovar <- list(NULL, covar0)

################################################################################

test_that("can be used with a subset of samples", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in sample(lcovar, 1)) {

      ind <- sample(N, N / 2)

      alpha <- runif(1, min = 1e-6, max = 1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr <- big_spLogReg(X, y, covar.train = covar, alphas = alpha,
                                    lambda.min = lambda.min)
      preds <- rowMeans(
        predict(mod.bigstatsr, X, ind.row = (1:N)[-ind], covar.row = covar[-ind, ])
      )
      expect_gt(AUC(preds, y[-ind]), 0.8)

      mod.bigstatsr2 <- big_spLogReg(X, y[ind], ind.train = ind,
                                     covar.train = covar[ind, ],
                                     alphas = alpha,
                                     lambda.min = lambda.min)
      preds2 <- rowMeans(
        predict(mod.bigstatsr2, X, ind.row = (1:N)[-ind],
                covar.row = covar[-ind, ])
      )
      expect_gt(AUC(preds2, y[-ind]), 0.7)
    }
  }
})

################################################################################

test_that("can be used with a subset of variables", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in sample(lcovar, 1)) {

      ind <- sample(N, N / 2)

      alpha <- runif(1, min = 1e-6, max = 1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr3 <- big_spLogReg(X, y[ind], ind.train = ind,
                                     ind.col = 6:M,
                                     covar.train = covar[ind, ],
                                     alphas = alpha,
                                     lambda.min = lambda.min)
      preds3 <- rowMeans(
        predict(mod.bigstatsr3, X, ind.row = (1:N)[-ind],
                covar.row = covar[-ind, ])
      )
      # Test that prediction is bad when removing the first variables
      expect_lt(AUC(preds3, y[-ind]), 0.6)
    }
  }
})

################################################################################

test_that("parameter 'return.all' works and loss computation is correct", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in sample(lcovar, 1)) {

      alpha <- runif(1, min = 1e-6, max = 1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr4 <- big_spLogReg(X, y, covar.train = covar, alphas = alpha,
                                     lambda.min = lambda.min, return.all = TRUE)

      expect_length(mod.bigstatsr4, 1)
      flatten <- unlist(mod.bigstatsr4, recursive = FALSE)
      expect_true(all(sapply(flatten, class) == "big_sp"))

      loss.val <- lapply(flatten, function(obj) {
        ind.val <- setdiff(rows_along(X), obj$ind.train)
        y.val <- y[ind.val]
        preds <- predict(obj, X, ind.row = ind.val, covar.row = covar[ind.val, ])
        preds_prob <- unname(1 / (1 + exp(-preds)))
        apply(preds_prob, 2, function(p) {
          -mean(y.val * log(p) + (1 - y.val) * log(1 - p))  ## negLogLik
        })
      })
      expect_equal(loss.val, lapply(flatten, function(obj) obj$loss.val))
    }
  }
})

################################################################################
