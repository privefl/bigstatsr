################################################################################

context("CROSSPROD_SELF")

# Simulating some data
N <- 101
M <- 43
x <- matrix(rnorm(N * M, 100, 5), N)

big_noscale <- big_scale(center = FALSE)

################################################################################

test_that("equality with crossprod", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    K <- big_crossprodSelf(X, fun.scaling = big_noscale,
                           block.size = 10)
    expect_equal(K[], crossprod(X[]))
  }
})

################################################################################

test_that("equality with crossprod with half of the data", {
  ind <- sample(M, M / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    # no scaling
    K <- big_crossprodSelf(X, fun.scaling = big_noscale, ind.col = ind,
                           block.size = 10)
    expect_equal(K[], crossprod(X[, ind]))

    # full scaling
    K2 <- big_crossprodSelf(X, fun.scaling = big_scale(), ind.col = ind,
                            block.size = 10)
    X.scaled <- scale(X[, ind])
    expect_equal(K2[], crossprod(X.scaled))
    expect_equal(attr(K2, "center"), attr(X.scaled, "scaled:center"))
    expect_equal(attr(K2, "scale"),  attr(X.scaled, "scaled:scale"))
  }
})

################################################################################

test_that("equality with crossprod with half of the data", {
  ind <- sample(N, N / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    # no scaling
    K <- big_crossprodSelf(X, fun.scaling = big_noscale, ind.row = ind,
                           block.size = 10)
    expect_equal(K[], crossprod(X[ind, ]))

    # full scaling
    K2 <- big_crossprodSelf(X, fun.scaling = big_scale(), ind.row = ind,
                            block.size = 10)
    X.scaled <- scale(X[ind, ])
    expect_equal(K2[], crossprod(X.scaled))
    expect_equal(attr(K2, "center"), attr(X.scaled, "scaled:center"))
    expect_equal(attr(K2, "scale"),  attr(X.scaled, "scaled:scale"))
  }
})

################################################################################
