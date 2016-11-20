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

    test <- big_tcrossprodSelf(X = X, fun.scaling = big_noscale,
                               use.Eigen = (runif(1) > 0.5))
    expect_equal(test[,], tcrossprod(X[,]))
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(N, N/2)
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)

    test <- big_tcrossprodSelf(X = X, fun.scaling = big_noscale,
                               ind.train = ind,
                               use.Eigen = (runif(1) > 0.5))
    expect_equal(test[,], tcrossprod(X[ind, ]))
  }
})

################################################################################

options(opt.save)

################################################################################
