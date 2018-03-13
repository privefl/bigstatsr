################################################################################

context("SP_LIN_REG")

# Simulating some data
N <- 530
M <- 730
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
s <- rowSums(x[, 1:10])
y <- s + rnorm(N)

covar0 <- matrix(rnorm(N * 3), N)
lcovar <- list(NULL, covar0)

################################################################################

test_that("can be used with a subset of samples", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in lcovar) {

      ind <- sample(N, N / 2)

      alpha <- runif(1, min = 1e-6, max = 1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr <- big_spLinReg(X, y, covar.train = covar, alpha = alpha,
                                    lambda.min = lambda.min)
      preds <- rowMeans(
        predict(mod.bigstatsr, X, ind.row = (1:N)[-ind], covar.row = covar[-ind, ])
      )
      expect_gt(cor(preds, y[-ind]), 0.8)

      mod.bigstatsr2 <- big_spLinReg(X, y[ind], ind.train = ind,
                                     covar.train = covar[ind, ],
                                     alpha = alpha,
                                     lambda.min = lambda.min)
      preds2 <- rowMeans(
        predict(mod.bigstatsr2, X, ind.row = (1:N)[-ind], covar.row = covar[-ind, ])
      )
      expect_gt(cor(preds2, y[-ind]), 0.5)
    }
  }
})

################################################################################

test_that("can be used with a subset of variables", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in lcovar) {

      ind <- sample(N, N / 2)

      alpha <- runif(1, min = 1e-6, max = 1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr3 <- big_spLinReg(X, y[ind], ind.train = ind,
                                     ind.col = 11:M,
                                     covar.train = covar[ind, ],
                                     alpha = alpha,
                                     lambda.min = lambda.min)
      preds3 <- rowMeans(
        predict(mod.bigstatsr3, X, ind.row = (1:N)[-ind], covar.row = covar[-ind, ])
      )
      # Test that prediction is bad (because not the first variable in the prediction)
      expect_lt(cor(preds3, y[-ind]), 0.2)
    }
  }
})

################################################################################

test_that("parameter 'return.all' works and loss computation is correct", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in lcovar) {

      alpha <- runif(1, min = 1e-6, max = 1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr4 <- big_spLinReg(X, y, covar.train = covar, alpha = alpha,
                                     lambda.min = lambda.min, return.all = TRUE)
      expect_true(all(sapply(mod.bigstatsr4, class) == "big_sp"))

      loss.val <- lapply(mod.bigstatsr4, function(obj) {
        ind.val <- setdiff(rows_along(X), obj$ind.train)
        y.val <- y[ind.val]
        preds <- predict(obj, X, ind.row = ind.val, covar.row = covar[ind.val, ])
        apply(unname(preds), 2, function(pred) mean((y.val - pred)^2))  ## MSE
      })
      expect_equal(loss.val, lapply(mod.bigstatsr4, function(obj) obj$loss.val))
    }
  }
})

################################################################################
