################################################################################

context("SP_SVM")

big_spSVM <- bigstatsr:::big_spSVM

# Simulating some data
N <- 511 # Some issues for small sample sizes
M <- 230
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
y <- sample(0:1, size = N, replace = TRUE)
y.factor <- factor(y, levels = c(1, 0))

covar0 <- matrix(rnorm(N * 3), N)
lcovar <- list(NULL, covar0)

################################################################################

test_that("equality with sparseSVM with all data", {
  skip_on_cran()
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in lcovar) {
      X2 <- cbind(X[], covar)
      m <- pmax(0, runif(ncol(X2), min = -0.5, max = 2))
      alpha <- runif(1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr <- big_spSVM(X, y, covar.train = covar, alpha = alpha,
                                 lambda.min = lambda.min, penalty.factor = m)
      mod.sparseSVM <- sparseSVM::sparseSVM(X2, y.factor,
                                            alpha = alpha,
                                            lambda.min = lambda.min,
                                            penalty.factor = m)

      expect_equal(mod.bigstatsr$lambda, mod.sparseSVM$lambda)
      expect_equivalent(as.matrix(mod.bigstatsr$beta),
                        mod.sparseSVM$weights[-1, ])
      expect_equal(mod.bigstatsr$intercept, mod.sparseSVM$weights[1, ])
    }
  }
})

################################################################################

test_that("equality with sparseSVM with only half the data", {
  skip_on_cran()
  ind <- sample(N, N / 2)
  while (mean(y[ind]) < 0.2 || mean(y[ind]) > 0.8) {
    ind <- sample(N, N / 2)
  }

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in lcovar) {
      X2 <- cbind(X[], covar)
      m <- pmax(0, runif(ncol(X2), min = -0.5, max = 2))
      alpha <- runif(1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr <- big_spSVM(X, y[ind], ind.train = ind,
                                 covar.train = covar[ind, ],
                                 alpha = alpha,
                                 lambda.min = lambda.min,
                                 penalty.factor = m)
      mod.sparseSVM <- sparseSVM::sparseSVM(X2[ind, ], y.factor[ind],
                                            alpha = alpha,
                                            lambda.min = lambda.min,
                                            penalty.factor = m)

      expect_equal(mod.bigstatsr$lambda, mod.sparseSVM$lambda)
      expect_equivalent(as.matrix(mod.bigstatsr$beta),
                        mod.sparseSVM$weights[-1, ])
      expect_equal(mod.bigstatsr$intercept, mod.sparseSVM$weights[1, ])
    }
  }
})

################################################################################
