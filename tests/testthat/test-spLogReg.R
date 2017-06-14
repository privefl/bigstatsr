################################################################################

context("SP_LOG_REG")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)


# Simulating some data
N <- 73
M <- 230
x <- matrix(rnorm(N * M, sd = 5), N)
y <- sample(0:1, size = N, replace = TRUE)

covar0 <- matrix(rnorm(N * 3), N)
lcovar <- list(NULL, covar0)

################################################################################

test_that("equality with biglasso with all data", {
  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    for (covar in lcovar) {
      X2 <- as.big.matrix(cbind(X[], covar), type = "double")
      m <- runif(ncol(X2), min = 0.5, max = 2)
      alpha <- runif(1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr <- big_spLogReg(X., y, covar.train = covar, alpha = alpha,
                                    lambda.min = lambda.min, penalty.factor = m)
      mod.biglasso <- biglasso::biglasso(X2, y,
                                         family = "binomial",
                                         alpha = alpha,
                                         penalty = "enet",
                                         lambda.min = lambda.min,
                                         penalty.factor = m)

      expect_equal(mod.bigstatsr$lambda, mod.biglasso$lambda)
      expect_equivalent(mod.bigstatsr$beta@x, mod.biglasso$beta[-1, ]@x)
      expect_equal(mod.bigstatsr$intercept, mod.biglasso$beta[1, ])
    }
  }
})

################################################################################

test_that("equality with biglasso with only half the data", {
  ind <- sample(N, N / 2)
  while (mean(y[ind]) < 0.2 || mean(y[ind]) > 0.8) {
    ind <- sample(N, N / 2)
  }

  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    for (covar in lcovar) {
      X2 <- as.big.matrix(cbind(X[], covar), type = "double")
      m <- runif(ncol(X2), min = 0.5, max = 2)
      alpha <- runif(1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr <- big_spLogReg(X., y[ind], ind.train = ind,
                                    covar.train = covar[ind, ],
                                    alpha = alpha,
                                    lambda.min = lambda.min,
                                    penalty.factor = m)
      mod.biglasso <- biglasso::biglasso(X2, y,
                                         family = "binomial",
                                         row.idx = ind,
                                         alpha = alpha,
                                         penalty = "enet",
                                         lambda.min = lambda.min,
                                         penalty.factor = m)

      expect_equal(mod.bigstatsr$lambda, mod.biglasso$lambda)
      expect_equivalent(mod.bigstatsr$beta@x, mod.biglasso$beta[-1, ]@x)
      expect_equal(mod.bigstatsr$intercept, mod.biglasso$beta[1, ])
    }
  }
})

################################################################################

options(opt.save)

################################################################################
