################################################################################

context("COUNTS")

set.seed(SEED)

################################################################################

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
X <- asFBMcode(x)

################################################################################

test_that("equality with table", {

  expect_error(big_counts(X, ind.row = NULL), "'ind.row' can't be `NULL`.")
  expect_error(big_counts(X, ind.col = NULL), "'ind.col' can't be `NULL`.")

  test2 <- big_counts(X)
  code2 <- as.numeric(rownames(test2))
  true2 <- apply(X[], 2, function(x)
    table(factor(x, levels = code2, exclude = NULL)))
  expect_equal(test2, true2)

  test1 <- big_counts(X, byrow = TRUE)
  code1 <- as.numeric(rownames(test1))
  true1 <- apply(X[], 1, function(x)
    table(factor(x, levels = code1, exclude = NULL)))
  expect_equal(test1, true1)
})

################################################################################

test_that("equality with table with half of the data", {
  ind <- sample(N, N / 2)

  test2 <- big_counts(X, ind.row = ind)
  code2 <- as.numeric(rownames(test2))
  true2 <- apply(X[ind, ], 2, function(x)
    table(factor(x, levels = code2, exclude = NULL)))
  expect_equal(test2, true2)

  test1 <- big_counts(X, ind.row = ind, byrow = TRUE)
  code1 <- as.numeric(rownames(test1))
  true1 <- apply(X[ind, ], 1, function(x)
    table(factor(x, levels = code1, exclude = NULL)))
  expect_equal(test1, true1)
})

################################################################################

test_that("equality with table with half of half of the data", {
  ind <- sample(N, N / 2)
  ind.col <- sample(M, M / 2)

  test2 <- big_counts(X, ind.row = ind, ind.col = ind.col)
  code2 <- as.numeric(rownames(test2))
  true2 <- apply(X[ind, ind.col], 2, function(x)
    table(factor(x, levels = code2, exclude = NULL)))
  expect_equal(test2, true2)

  test1 <- big_counts(X, ind.row = ind, ind.col = ind.col, byrow = TRUE)
  code1 <- as.numeric(rownames(test1))
  true1 <- apply(X[ind, ind.col], 1, function(x)
    table(factor(x, levels = code1, exclude = NULL)))
  expect_equal(test1, true1)
})

################################################################################
