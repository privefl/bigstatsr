################################################################################

context("CHECK_X")

mat <- matrix(0, 10, 10)
X <- as.big.matrix(mat)
y.class <- sample(c(-1, 1), size = 10, replace = TRUE)
y.reg <- rnorm(10)

################################################################################

test_that("is a big.matrix?", {
  expect_error(check_X(mat), ERROR_BIGMATRIX, fixed = TRUE)
  expect_null(check_X(X))
})

################################################################################

test_that("y == class?", {
  expect_error(check_X(X, y = y.reg, y.type = "class"),
               ERROR_CLASS, fixed = TRUE)
  expect_null(check_X(X, y = y.class, y.type = "class"))
})

################################################################################

test_that("y == reg?", {
  expect_error(check_X(X, y = y.class, y.type = "reg"),
               ERROR_REG, fixed = TRUE)
  expect_null(check_X(X, y = y.reg, y.type = "reg"))
})

################################################################################
