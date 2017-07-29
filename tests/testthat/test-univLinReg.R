################################################################################

context("UNIV_LIN_REG")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N * M, sd = 5), N)
y <- rnorm(N)

covar0 <- matrix(rnorm(N * 3), N)
lcovar <- list(NULL, covar0)

################################################################################

getLM <- function(X, y, covar, ind = NULL) {
  res <- matrix(NA, M, 4)
  for (i in 1:M) {
    mod <- lm(y ~ cbind(X[, i, drop = FALSE], covar), subset = ind)
    res[i, ] <- summary(mod)$coefficients[2, ]
  }
  res
}

################################################################################

test_that("equality with lm with all data", {
  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    for (covar in lcovar) {
      mod <- big_univLinReg(X., y, covar.train = covar, ncores = sample(1:2, 1))
      mod$p.value <- predict(mod, log10 = FALSE)
      expect_equivalent(as.matrix(mod), getLM(X, y, covar))
    }
  }
})

################################################################################

test_that("equality with lm with only half the data", {
  ind <- sample(N, N / 2)

  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    for (covar in lcovar) {
      mod <- big_univLinReg(X., y.train = y[ind],
                            covar.train = covar[ind, ],
                            ind.train = ind,
                            ncores = sample(1:2, 1))
      mod$p.value <- predict(mod, log10 = FALSE)
      expect_equivalent(as.matrix(mod), getLM(X, y, covar, ind))
    }
  }
})

################################################################################

plot(mod, type = "Manhattan")
plot(mod, type = "Q-Q")
plot(mod, type = "Volcano")

################################################################################

options(opt.save)

################################################################################
