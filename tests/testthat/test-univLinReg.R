################################################################################

context("UNIV_LIN_REG")

set.seed(SEED)

################################################################################

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
y <- rnorm(N)
y2 <- (y > 0)
y3 <- y; y3[] <- 0

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
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in lcovar) {

      expect_error(big_univLinReg(X, y3, covar.train = covar, ncores = test_cores()),
                     "'y.train' should be composed of different values.", fixed = TRUE)
      expect_warning(big_univLinReg(X, y2, covar.train = covar, ncores = test_cores()),
                     "'y.train' is composed of only two different levels.", fixed = TRUE)

      mod <- big_univLinReg(X, y, covar.train = covar, ncores = test_cores())
      mod$p.value <- predict(mod, log10 = FALSE)
      expect_equivalent(as.matrix(mod), getLM(X, y, covar))

      p <- plot(mod, type = sample(c("Manhattan", "Q-Q", "Volcano"), 1))
      expect_s3_class(p, "ggplot")

      expect_error(predict(mod, abc = 2), "Argument 'abc' not used.")
      expect_error(plot(mod, abc = 2), "Argument 'abc' not used.")
    }
  }
})

################################################################################

test_that("equality with lm with only half the data", {
  ind <- sample(N, N / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (covar in lcovar) {
      mod <- big_univLinReg(X, y.train = y[ind],
                            covar.train = covar[ind, ],
                            ind.train = ind,
                            ncores = test_cores())
      mod$p.value <- predict(mod, log10 = FALSE)
      expect_equivalent(as.matrix(mod), getLM(X, y, covar, ind))

      p <- plot(mod, type = sample(c("Manhattan", "Q-Q", "Volcano"), 1))
      expect_s3_class(p, "ggplot")
    }
  }
})

################################################################################

test_that("covar_from_df() works", {

  iris2 <- datasets::iris
  mat <- covar_from_df(iris2)
  expect_type(mat, "double")
  expect_equal(nrow(mat), nrow(iris2))
  expect_equal(qr(mat)$rank, ncol(mat))

  names(iris2) <- NULL
  mat <- covar_from_df(iris2)
  expect_type(mat, "double")
  expect_equal(nrow(mat), nrow(iris2))
  expect_equal(qr(mat)$rank, ncol(mat))

  iris2[c(1, 3, 57), ] <- NA
  mat2 <- covar_from_df(iris2)
  mat[c(1, 3, 57), ] <- NA
  expect_equal(mat2, mat)
})

################################################################################
