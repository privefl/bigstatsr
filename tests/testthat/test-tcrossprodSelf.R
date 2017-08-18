################################################################################

context("TCROSSPROD_SELF")

# Simulating some data
N <- 43
M <- 101
x <- matrix(rnorm(N * M, 100, 5), N)

big_noscale <- big_scale(center = FALSE)

################################################################################

test_that("equality with tcrossprod", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    K <- big_tcrossprodSelf(X, fun.scaling = big_noscale)
    expect_equivalent(K, tcrossprod(X[]))
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(M, M / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    # no scaling
    K <- big_tcrossprodSelf(X, fun.scaling = big_noscale, ind.col = ind)
    expect_equivalent(K, tcrossprod(X[, ind]))

    # full scaling
    K2 <- big_tcrossprodSelf(X, fun.scaling = big_scale(), ind.col = ind)
    expect_equivalent(K2, tcrossprod(scale(X[, ind])))
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(N, N / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    # no scaling
    K <- big_tcrossprodSelf(X, fun.scaling = big_noscale, ind.row = ind)
    expect_equivalent(K, tcrossprod(X[ind, ]))

    # full scaling
    K2 <- big_tcrossprodSelf(X, fun.scaling = big_scale(), ind.row = ind)
    expect_equivalent(K2, tcrossprod(scale(X[ind, ])))
  }
})

################################################################################
