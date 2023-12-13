################################################################################

context("CROSSPROD_SELF")

set.seed(SEED)

################################################################################

# Simulating some data
N <- 101
M <- 43
x <- matrix(rnorm(N * M, 100, 5), N)

################################################################################

test_that("equality with crossprod", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    K <- big_crossprodSelf(X, block.size = 10)
    expect_equal(K[], crossprod(X[]))

    if (t == "double") {
      expect_equal(crossprod(X), crossprod(X[]))
    } else {
      expect_error(crossprod(X, "for 'double' FBMs only"))
    }
  }
})

################################################################################

test_that("equality with crossprod with half of the data", {
  ind <- sample(M, M / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    # no scaling
    K <- big_crossprodSelf(X, ind.col = ind, block.size = 10)
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
    tmp <- tempfile()
    K <- big_crossprodSelf(X, ind.row = ind, block.size = 10, backingfile = tmp)
    expect_equal(K[], crossprod(X[ind, ]))
    expect_identical(K$backingfile, normalizePath(paste0(tmp, ".bk")))

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
