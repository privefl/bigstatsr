################################################################################

context("REGLIN")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
N1 <- 3000
N2 <- 1000
x1 <- rnorm(N1, 0.5)
x2 <- rnorm(N2, -0.5)
weights <- c(rep(N2, N1), rep(N1, N2))
weights2 <- rep(1, N1 + N2)

x <- c(x1, x2)
x3 <- cbind(x, x + 1, 2*x)

################################################################################

get_res_weights <- function(X, y, weighted = FALSE) {
  res <- matrix(0, 3, ncol(X))
  for (j in 1:ncol(X)) {
    mylm <- lm(y ~ X[, j], weights = `if`(weighted, weights, weights2))
    res[1:2, j] <- mylm$coefficients
    res[3, j] <- summary(mylm)$r.squared
  }
  res
}

get_res2_class <- function(X, y, weighted = FALSE) {
  rbind(CoeffsClass(X, y, weighted = weighted),
        RsqClass(X, y, weighted = weighted))
}

# In a case of classification
y <- c(rep(1, N1), rep(-1, N2))

test_that("equality with lm in case of classification", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x3, type = t)
    for (w in c(TRUE, FALSE)) {
      expect_equivalent(get_res2_class(X, y, weighted = w),
                        get_res_weights(X, y, weighted = w))
    }
  }
})

test_that("expect error from regression", {
  X <- as.big.matrix(x3)
  expect_error(CoeffsReg(X, y), ERROR_REG, fixed = TRUE)
  expect_error(RsqReg(X, y), ERROR_REG, fixed = TRUE)
})

################################################################################

get_res <- function(X, y) {
  res <- matrix(0, 3, ncol(X))
  for (j in 1:ncol(X)) {
    mylm <- lm(y ~ X[, j])
    res[1:2, j] <- mylm$coefficients
    res[3, j] <- summary(mylm)$r.squared
  }
  res
}

get_res2_reg <- function(X, y) {
  rbind(CoeffsReg(X, y), RsqReg(X, y))
}

# In a case of regression
y2 <- x + rnorm(length(x), 0, 0.1)

test_that("equality with lm in case of regression", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x3, type = t)
    expect_equivalent(get_res2_reg(X, y2), get_res(X, y2))
  }
})

test_that("expect error from classification", {
  X <- as.big.matrix(x3)
  expect_error(CoeffsClass(X, y2), ERROR_CLASS, fixed = TRUE)
  expect_error(RsqClass(X, y2), ERROR_CLASS, fixed = TRUE)
})

################################################################################

ind.train <- sort(sample(length(x), length(x) / 2))

get_res_train <- function(X, y) {
  res <- matrix(0, 3, ncol(X))
  for (j in 1:ncol(X)) {
    mylm <- lm(y[ind.train] ~ X[ind.train, j])
    res[1:2, j] <- mylm$coefficients
    res[3, j] <- summary(mylm)$r.squared
  }
  res
}

get_res2_reg_train <- function(X, y) {
  rbind(CoeffsReg(X, y, ind.train), RsqReg(X, y, ind.train))
}

# With only half of the data
test_that("equality with lm in case of regression with half of the data", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x3, type = t)
    expect_equivalent(get_res2_reg_train(X, y2), get_res_train(X, y2))
  }
})

################################################################################

test_that("Expect error from unknown type", {
  x <- as.raw(sample(0:255, 100))
  X <- as.big.matrix(matrix(x), type = "raw")
  expect_error(CoeffsClass(X, y), ERROR_TYPE, fixed = TRUE)
  expect_error(RsqClass(X, y), ERROR_TYPE, fixed = TRUE)
})

################################################################################

options(opt.save)

################################################################################
