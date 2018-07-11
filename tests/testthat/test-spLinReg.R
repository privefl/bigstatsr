################################################################################

context("SP_LIN_REG")

set.seed(SEED)

################################################################################

# Simulating some data
N <- 934
M <- 1053
m <- 30
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
set <- sample(M, size = m)
eff <- rnorm(m)
s <- drop(scale(x[, set]) %*% eff) / m
y <- s + rnorm(N) / 10
y2 <- (y > 0)
y3 <- y; y3[] <- 0

covar0 <- matrix(rnorm(N * 3), N)
lcovar <- list(NULL, covar0)

################################################################################

test_that("can be used with a subset of samples", {

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in sample(lcovar, 1)) {

      expect_error(big_spLinReg(X, y3, covar.train = covar, ncores = test_cores()),
                   "'y.train' should be composed of different values.", fixed = TRUE)
      expect_warning(big_spLinReg(X, y2, covar.train = covar, ncores = test_cores()),
                     "'y.train' is composed of only two different levels.", fixed = TRUE)

      ind <- sample(N, N / 2)
      alphas <- c(runif(1, min = 0.01, max = 1), 1)

      mod.bigstatsr <- big_spLinReg(X, y, covar.train = covar,
                                    alphas = alphas,
                                    ncores = test_cores())
      preds <- predict(mod.bigstatsr, X, ind.row = (1:N)[-ind],
                       covar.row = covar[-ind, ])
      expect_gt(cor(preds, y[-ind]), 0.75)

      mod.bigstatsr2 <- big_spLinReg(X, y[ind], ind.train = ind,
                                     covar.train = covar[ind, ],
                                     alphas = alphas,
                                     ncores = test_cores())
      preds2 <- predict(mod.bigstatsr2, X, ind.row = (1:N)[-ind],
                        covar.row = covar[-ind, ])
      expect_gt(cor(preds2, y[-ind]), 0.6)

      expect_error(predict(mod.bigstatsr2, X, covar.row = covar, abc = 2),
                   "Argument 'abc' not used.")

    }
  }
})

################################################################################

test_that("can be used with a subset of variables", {

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in sample(lcovar, 1)) {

      ind <- sample(N, N / 2)
      ind.col <- cols_along(X)[-set]

      alpha <- runif(1, min = 0.1, max = 1)

      mod.bigstatsr3 <- big_spLinReg(X, y[ind], ind.train = ind,
                                     ind.col = ind.col,
                                     covar.train = covar[ind, ],
                                     alphas = alpha,
                                     ncores = test_cores())
      preds3 <- predict(mod.bigstatsr3, X, ind.row = (1:N)[-ind],
                        covar.row = covar[-ind, ])
      # Test that prediction is bad when removing the first variables
      if (any(diff(preds3) != 0)) expect_lt(cor(preds3, y[-ind]), 0.2)
    }
  }
})

################################################################################

test_that("parameter 'return.all' works and loss computation is correct", {

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in sample(lcovar, 1)) {

      alpha <- runif(1, min = 0.01, max = 1)

      mod.bigstatsr4 <- big_spLinReg(X, y, covar.train = covar,
                                     alphas = alpha,
                                     return.all = TRUE,
                                     ncores = test_cores())

      expect_length(mod.bigstatsr4, 1)
      flatten <- mod.bigstatsr4[[1]]
      expect_true(all(sapply(flatten, class) == "big_sp"))

      lapply(flatten, function(obj) {
        ind.val <- setdiff(rows_along(X), obj$ind.train)
        y.val <- y[ind.val]
        preds <- predict(obj, X, ind.row = ind.val, covar.row = covar[ind.val, ])
        loss.val <- mean((y.val - preds)^2)  ## MSE
        diff <- abs(loss.val - obj$loss.val)
        expect_true(any(diff < 1e-8))
      })
    }
  }
})

################################################################################

test_that("Use a base predictor", {

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in sample(lcovar, 1)) {

      ind <- sample(N, N / 2)
      alphas <- c(runif(1, min = 0.01, max = 1), 1)
      ind.sets <- sample(rep_len(1:10, length(ind)))

      mod.bigstatsr <- big_spLinReg(X, y[ind], ind.train = ind,
                                    covar.train = covar[ind, ],
                                    alphas = alphas,
                                    ind.sets = ind.sets,
                                    ncores = test_cores())
      preds <- predict(mod.bigstatsr, X, covar.row = covar)
      expect_gt(cor(preds[-ind], y[-ind]), 0.6)

      mod.bigstatsr2 <- big_spLinReg(X, y[ind], ind.train = ind,
                                     base.train = rep(10, length(ind)),
                                     covar.train = covar[ind, ],
                                     alphas = alphas,
                                     ind.sets = ind.sets,
                                     ncores = test_cores())
      expect_equal(sapply(mod.bigstatsr2, function(x) x$intercept) + 10,
                   sapply(mod.bigstatsr, function(x) x$intercept))
      expect_equal(sapply(mod.bigstatsr2, function(x) x$beta.X),
                   sapply(mod.bigstatsr, function(x) x$beta.X))

      mod.bigstatsr3 <- big_spLinReg(X, y[ind], ind.train = ind,
                                     base.train = preds[ind] / 2,
                                     covar.train = covar[ind, ],
                                     alphas = alphas,
                                     ind.sets = ind.sets,
                                     ncores = test_cores())
      preds3 <- predict(mod.bigstatsr3, X, covar.row = covar)
      expect_equal(preds3, preds / 2, tolerance = 0.1)
    }
  }
})

################################################################################
