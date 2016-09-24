################################################################################

context("TRANSPOSE")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
x <- matrix(rnorm(100, 0, 5), 20, 5)

################################################################################

test_that("Equality with t()", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)

    test <- transpose(X)
    expect_identical(test[,], t(X[,]))
  }
})

################################################################################

options(opt.save)

################################################################################
