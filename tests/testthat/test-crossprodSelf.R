################################################################################

context("CROSSPROD_SELF")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
N <- 101
M <- 43
x <- matrix(rnorm(N*M), N)

big_noscale <- big_scale(center = FALSE)

################################################################################

test_that("equality with crossprod", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)

    test <- big_crossprodSelf(X = X, fun.scaling = big_noscale,
                              use.Eigen = (runif(1) > 0.5))
    expect_equal(test$K, crossprod(X[,]))
  }
})

################################################################################

test_that("equality with crossprod with half of the data", {
  ind <- sample(N, N/2)
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)

    test <- big_crossprodSelf(X = X, fun.scaling = big_noscale,
                              ind.train = ind,
                              use.Eigen = (runif(1) > 0.5))
    expect_equal(test$K, crossprod(X[ind, ]))
  }
})

################################################################################

options(opt.save)

################################################################################
