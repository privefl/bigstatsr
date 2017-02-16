################################################################################

context("SP_REG_LIN")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)


# Simulating some data
N <- 73
M <- 230
x <- matrix(rnorm(N*M, sd = 5), N)
y <- rnorm(N)

covar0 <- matrix(rnorm(N * 3), N)
lcovar <- list(NULL, covar0)

################################################################################

test_that("equality with biglasso with all data", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    for (covar in lcovar) {
      X2 <- as.big.matrix(cbind(X[,], covar), type = "double")
      m <- runif(ncol(X2), min = 0.5, max = 2)
      alpha <- runif(1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr <- big_spRegLin(X, y, covar.train = covar, alpha = alpha,
                                    lambda.min = lambda.min, penalty.factor = m)
      mod.biglasso <- biglasso::biglasso(X2, y,
                                         family = "gaussian",
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

  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    for (covar in lcovar) {
      X2 <- as.big.matrix(cbind(X[,], covar), type = "double")
      m <- runif(ncol(X2), min = 0.5, max = 2)
      alpha <- runif(1)
      lambda.min <- runif(1, min = 0.01, max = 0.5)

      mod.bigstatsr <- big_spRegLin(X, y[ind], ind.train = ind,
                                    covar.train = covar[ind, ],
                                    alpha = alpha,
                                    lambda.min = lambda.min,
                                    penalty.factor = m)
      mod.biglasso <- biglasso::biglasso(X2, y,
                                         family = "gaussian",
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
