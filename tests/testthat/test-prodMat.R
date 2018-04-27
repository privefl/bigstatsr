################################################################################

context("PROD_MAT")

set.seed(SEED)

################################################################################

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)

################################################################################

for (t in TEST.TYPES) {
  X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

  test_that("equality with %*%", {
    replicate(20, {
      n <- sample(N, size = 1)
      m <- sample(M, size = 1)
      ind.row <- sample(N, size = n)
      ind.col <- sample(M, size = m)
      A.col <- matrix(rnorm(n * m), m, n)
      expect_equal(big_prodMat(X, A.col, ind.row, ind.col),
                   X[ind.row, ind.col, drop = FALSE] %*% A.col)
      A.row <- matrix(rnorm(n * m), n, m)
      expect_equal(big_cprodMat(X, A.row, ind.row, ind.col),
                   crossprod(X[ind.row, ind.col, drop = FALSE], A.row))
    })
  })

  test_that("Incompatiblity between dimensions", {
    ind.row <- sample(N, size = 21)
    ind.col <- sample(M, size = 11)
    A.col <- matrix(1, 21, 11)
    expect_error(big_prodMat(X, A.col, ind.row, ind.col),
                 "Incompatibility between dimensions.")
    A.row <- matrix(1, 11, 21)
    expect_error(big_cprodMat(X, A.row, ind.row, ind.col),
                 "Incompatibility between dimensions.")
  })

  test_that("OK with dimension 1", {
    ind.row <- sample(N, size = 17)
    ind.col <- sample(M, size = 1)
    A.col <- matrix(1, 1, 7)
    expect_equal(dim(big_prodMat(X, A.col, ind.row, ind.col)), c(17, 7))
    A.row <- matrix(1, 17, 7)
    expect_equal(dim(big_cprodMat(X, A.row, ind.row, ind.col)), c(1, 7))

    ind.row <- sample(N, size = 1)
    ind.col <- sample(M, size = 17)
    A.col <- matrix(1, 17, 7)
    expect_equal(dim(big_prodMat(X, A.col, ind.row, ind.col)), c(1, 7))
    A.row <- matrix(1, 1, 7)
    expect_equal(dim(big_cprodMat(X, A.row, ind.row, ind.col)), c(17, 7))
  })
}

################################################################################
