################################################################################

context("UNIV_REG_LIN")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
N1 <- 3000
N2 <- 1000
x1 <- rnorm(N1, 0.5)
x2 <- rnorm(N2, -0.5)

x <- c(x1, x2)
x3 <- cbind(x, x + 1, 2*x)

################################################################################

get_res_weights <- function(X, y) {
  res <- matrix(0, 3, ncol(X))
  for (j in 1:ncol(X)) {
    mylm <- lm(y ~ X[, j])
    res[1:2, j] <- mylm$coefficients
    res[3, j] <- summary(mylm)$r.squared
  }
  res
}

# In a case of classification
y <- c(rep(1, N1), rep(-1, N2))

test_that("equality with lm in case of classification", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x3, type = t)
    expect_equivalent(big_univRegLin(X, y), get_res_weights(X, y))
  }
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

# In a case of regression
y2 <- x + rnorm(length(x), 0, 0.1)

test_that("equality with lm in case of regression", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x3, type = t)
    expect_equivalent(big_univRegLin(X, y2), get_res(X, y2))
  }
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

# With only half of the data
test_that("equality with lm in case of regression with half of the data", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x3, type = t)
    expect_equivalent(big_univRegLin(X, y2, ind.train), get_res_train(X, y2))
  }
})

################################################################################

test_that("Expect error from unknown type", {
  x <- as.raw(sample(0:255, 100))
  X <- as.big.matrix(matrix(x), type = "raw")
  expect_error(big_univRegLin(X, y), ERROR_TYPE, fixed = TRUE)
})

################################################################################

options(opt.save)

################################################################################
