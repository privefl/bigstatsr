################################################################################

context("COR")

# Simulating some data
N <- 101
M <- 43
x <- matrix(rnorm(N * M, 100, 5), N)

################################################################################

test_that("equality with cor", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    K <- big_cor(X)
    expect_equivalent(K, cor(X[]))
  }
})

################################################################################

test_that("equality with cor with half of the data", {
  ind <- sample(M, M / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    K <- big_cor(X, ind.col = ind)
    expect_equivalent(K, cor(X[, ind]))
  }
})

################################################################################

test_that("equality with cor with half of the data", {
  ind <- sample(N, N / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    K <- big_cor(X, ind.row = ind)
    expect_equivalent(K, cor(X[ind, ]))
  }
})

################################################################################
