################################################################################

context("CROSSPROD_SELF")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)

# Simulating some data
N <- 101
M <- 43
x <- matrix(rnorm(N * M), N)

big_noscale <- big_scale(center = FALSE)

################################################################################

test_that("equality with crossprod", {
  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    K <- big_crossprodSelf(X., fun.scaling = big_noscale)
    expect_equivalent(K, crossprod(X[]))
  }
})

################################################################################

test_that("equality with crossprod with half of the data", {
  ind <- sample(M, M / 2)

  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    # no scaling
    K <- big_crossprodSelf(X., fun.scaling = big_noscale, ind.col = ind)
    expect_equivalent(K, crossprod(X[, ind]))

    # full scaling
    K2 <- big_crossprodSelf(X., fun.scaling = big_scale(), ind.col = ind)
    expect_equivalent(K2, crossprod(scale(X[, ind])))
  }
})

################################################################################

test_that("equality with crossprod with half of the data", {
  ind <- sample(N, N / 2)

  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    # no scaling
    K <- big_crossprodSelf(X., fun.scaling = big_noscale, ind.row = ind)
    expect_equivalent(K, crossprod(X[ind, ]))

    # full scaling
    K2 <- big_crossprodSelf(X., fun.scaling = big_scale(), ind.row = ind)
    expect_equivalent(K2, crossprod(scale(X[ind, ])))
  }
})

################################################################################

options(opt.save)

################################################################################
