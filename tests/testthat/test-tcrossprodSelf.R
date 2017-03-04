################################################################################

context("TCROSSPROD_SELF")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)

# Simulating some data
N <- 43
M <- 101
x <- matrix(rnorm(N*M), N)

big_noscale <- big_scale(center = FALSE)

################################################################################

test_that("equality with tcrossprod", {
  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    test <- big_tcrossprodSelf(X., fun.scaling = big_noscale)
    expect_equal(test$K, tcrossprod(X[,]))
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(N, N/2)

  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    test <- big_tcrossprodSelf(X., fun.scaling = big_noscale,
                               ind.row = ind)
    expect_equal(test$K, tcrossprod(X[ind, ]))
  }
})

################################################################################

options(opt.save)

################################################################################
