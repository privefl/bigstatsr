################################################################################

context("TCROSSPROD_SELF")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)

# Simulating some data
N <- 43
M <- 101
x <- matrix(rnorm(N * M), N)

big_noscale <- big_scale(center = FALSE)

################################################################################

test_that("equality with tcrossprod", {
  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    K <- big_tcrossprodSelf(X., fun.scaling = big_noscale)
    expect_equivalent(K, tcrossprod(X[]))
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(M, M / 2)

  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    # no scaling
    K <- big_tcrossprodSelf(X., fun.scaling = big_noscale, ind.col = ind)
    expect_equivalent(K, tcrossprod(X[, ind]))

    # full scaling
    K2 <- big_tcrossprodSelf(X., fun.scaling = big_scale(), ind.col = ind)
    expect_equivalent(K2, tcrossprod(scale(X[, ind])))
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(N, N / 2)

  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    # no scaling
    K <- big_tcrossprodSelf(X., fun.scaling = big_noscale, ind.row = ind)
    expect_equivalent(K, tcrossprod(X[ind, ]))

    # full scaling
    K2 <- big_tcrossprodSelf(X., fun.scaling = big_scale(), ind.row = ind)
    expect_equivalent(K2, tcrossprod(scale(X[ind, ])))
  }
})

################################################################################

options(opt.save)

################################################################################
