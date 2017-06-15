################################################################################

context("COR")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)

# Simulating some data
N <- 101
M <- 43
x <- matrix(rnorm(N * M), N)

################################################################################

test_that("equality with cor", {
  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    K <- big_cor(X.)
    expect_equivalent(K, cor(X[]))
  }
})

################################################################################

test_that("equality with cor with half of the data", {
  ind <- sample(M, M / 2)

  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    K <- big_cor(X., ind.col = ind)
    expect_equivalent(K, cor(X[, ind]))
  }
})

################################################################################

test_that("equality with cor with half of the data", {
  ind <- sample(N, N / 2)

  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    K <- big_cor(X., ind.row = ind)
    expect_equivalent(K, cor(X[ind, ]))
  }
})

################################################################################

options(opt.save)

################################################################################
