################################################################################

context("TCROSSPROD_SELF")

# Simulating some data
N <- 43
M <- 101
x <- matrix(rnorm(N * M, 100, 5), N)

################################################################################

test_that("equality with tcrossprod", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    K <- big_tcrossprodSelf(X, block.size = 10)
    expect_equal(K[], tcrossprod(X[]))
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(M, M / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    # no scaling
    K <- big_tcrossprodSelf(X, ind.col = ind, block.size = 10)
    expect_equal(K[], tcrossprod(X[, ind]))

    # full scaling
    K2 <- big_tcrossprodSelf(X, fun.scaling = big_scale(), ind.col = ind,
                             block.size = 10)
    X.scaled <- scale(X[, ind])
    expect_equal(K2[], tcrossprod(X.scaled))
    expect_equal(attr(K2, "center"), attr(X.scaled, "scaled:center"))
    expect_equal(attr(K2, "scale"),  attr(X.scaled, "scaled:scale"))
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(N, N / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    # no scaling
    K <- big_tcrossprodSelf(X, ind.row = ind, block.size = 10)
    expect_equal(K[], tcrossprod(X[ind, ]))

    # full scaling
    K2 <- big_tcrossprodSelf(X, fun.scaling = big_scale(), ind.row = ind,
                             block.size = 10)
    X.scaled <- scale(X[ind, ])
    expect_equal(K2[], tcrossprod(X.scaled))
    expect_equal(attr(K2, "center"), attr(X.scaled, "scaled:center"))
    expect_equal(attr(K2, "scale"),  attr(X.scaled, "scaled:scale"))
  }
})

################################################################################
