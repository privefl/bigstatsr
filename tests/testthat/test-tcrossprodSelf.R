################################################################################

context("TCROSSPROD_SELF")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
N <- 43
M <- 101
x <- matrix(rnorm(N*M), N)

big_noscale <- big_scale(center = FALSE)

################################################################################

test_that("equality with tcrossprod", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)

    test <- big_tcrossprodSelf(X = X, fun.scaling = big_noscale)
    expect_equal(test$K, tcrossprod(X[,]))
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(N, N/2)

  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)

    test <- big_tcrossprodSelf(X = X, fun.scaling = big_noscale,
                               ind.train = ind)
    expect_equal(test$K, tcrossprod(X[ind, ]))
  }
})

################################################################################

options(opt.save)

################################################################################
