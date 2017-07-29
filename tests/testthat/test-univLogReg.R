################################################################################

context("UNIV_LOG_REG")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)

TOL <- 1e-5

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N * M, sd = 5), N)
y <- sample(0:1, size = N, replace = TRUE)

covar0 <- matrix(rnorm(N * 3), N)
lcovar <- list(NULL, covar0)

################################################################################

getGLM <- function(X, y, covar, ind = NULL) {
  res <- matrix(NA, M, 4)
  for (i in 1:M) {
    mod <- glm(y ~ cbind(X[, i, drop = FALSE], covar),
               family = "binomial", subset = ind,
               control = list(epsilon = 1e-10, maxit = 100))
    if (mod$converged)
      res[i, ] <- summary(mod)$coefficients[2, ]
  }
  res
}

################################################################################

test_that("equality with glm with all data", {
  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))
    for (covar in lcovar) {
      mod <- big_univLogReg(X., y, covar.train = covar)
      mod$p.value <- predict(mod, log10 = FALSE)
      mat <- as.matrix(mod[, -3])
      dimnames(mat) <- NULL
      expect_equal(mat, getGLM(X, y, covar), tolerance = TOL)

      p <- plot(mod, type = sample(c("Manhattan", "Q-Q", "Volcano"), 1))
      expect_s3_class(p, "ggplot")
    }
  }
})

################################################################################

test_that("equality with glm with only half the data", {
  ind <- sample(N, N / 2)
  while (mean(y[ind]) < 0.2 || mean(y[ind]) > 0.8) {
    ind <- sample(N, N / 2)
  }

  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))
    for (covar in lcovar) {
      mod <- big_univLogReg(X., y[ind],
                            covar.train = covar[ind, ],
                            ind.train = ind)
      mod$p.value <- predict(mod, log10 = FALSE)
      mat <- as.matrix(mod[, -3])
      dimnames(mat) <- NULL
      expect_equal(mat, getGLM(X, y, covar, ind), tolerance = TOL) # FAIL HERE

      p <- plot(mod, type = sample(c("Manhattan", "Q-Q", "Volcano"), 1))
      expect_s3_class(p, "ggplot")
    }
  }
})

################################################################################

options(opt.save)

################################################################################
