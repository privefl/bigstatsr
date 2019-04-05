################################################################################

context("UNIV_LOG_REG")

set.seed(SEED)

################################################################################

# Bug when no variation -> glm drop variable and intercept is returned first...
expect_warning(expect_message(
  gwas <- big_univLogReg(FBM(4, 1, init = 0), c(0, 1, 1, 1))))
expect_true(is.na(gwas$score))

################################################################################

TOL <- 1e-4

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
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

test_that("numerical problems", {
  # skip_on_appveyor(); skip_on_travis()
  X <- big_copy(x, type = "double")
  covar <- cbind(covar0, x[, 1:5])
  expect_warning(expect_message(
    mod <- big_univLogReg(X, y, covar.train = covar, ncores = test_cores()),
    "For 5 columns"),"For 5 columns")
  mod$p.value <- predict(mod, log10 = FALSE)
  mat <- as.matrix(mod[, -3])
  dimnames(mat) <- NULL
  expect_true(all(is.na(mat[1:5, ])))
  expect_equal(mat[-(1:5), ], getGLM(X, y, covar)[-(1:5), ], tolerance = TOL)

  covar2 <- cbind(covar, x[, 1])
  expect_error(
    big_univLogReg(X, y, covar.train = covar2, ncores = test_cores()),
    "'covar.train' is singular.", fixed = TRUE)
})

################################################################################

test_that("equality with glm with all data", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in lcovar) {
      mod <- big_univLogReg(X, y, covar.train = covar, ncores = test_cores())
      mod$p.value <- predict(mod, log10 = FALSE)
      mat <- as.matrix(mod[, -3])
      dimnames(mat) <- NULL
      expect_equal(mat, getGLM(X, y, covar), tolerance = TOL)

      p <- plot(mod, type = sample(c("Manhattan", "Q-Q", "Volcano"), 1))
      expect_s3_class(p, "ggplot")

      expect_error(predict(mod, abc = 2), "Argument 'abc' not used.")
      expect_error(plot(mod, abc = 2), "Argument 'abc' not used.")
    }
  }
})

################################################################################

test_that("equality with glm with only half the data", {
  ind <- sample(N, N / 2)
  while (mean(y[ind]) < 0.2 || mean(y[ind]) > 0.8) {
    ind <- sample(N, N / 2)
  }

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in lcovar) {
      mod <- big_univLogReg(X, y[ind],
                            covar.train = covar[ind, ],
                            ind.train = ind,
                            ncores = test_cores())
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
