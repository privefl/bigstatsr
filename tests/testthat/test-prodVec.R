################################################################################

context("PROD_VEC")

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
      y.col <- rnorm(m)
      expect_equal(big_prodVec(X, y.col, ind.row, ind.col),
                   drop(X[ind.row, ind.col, drop = FALSE] %*% y.col))
      y.row <- rnorm(n)
      expect_equal(big_cprodVec(X, y.row, ind.row, ind.col),
                   drop(crossprod(X[ind.row, ind.col, drop = FALSE], y.row)))
    })
  })

  test_that("Incompatiblity between dimensions", {
    ind.row <- sample(N, size = 21)
    ind.col <- sample(M, size = 11)
    y.col <- rnorm(21)
    expect_error(big_prodVec(X, y.col, ind.row, ind.col),
                 "Incompatibility between dimensions.")
    y.row <- rnorm(11)
    expect_error(big_cprodVec(X, y.row, ind.row, ind.col),
                 "Incompatibility between dimensions.")
  })
}

################################################################################
