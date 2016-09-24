################################################################################

context("TRANSPOSE")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
x <- matrix(rnorm(2e4, 0, 5), 200, 100)

################################################################################

test_that("Equality with t()", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)

    test <- transpose(X)
    expect_identical(test[,], t(X[,]))
  }
})

################################################################################

test_that("Expect error from unknown type", {
  x <- as.raw(sample(0:255, 100))
  X <- as.big.matrix(matrix(x), type = "raw")
  expect_error(transpose(X), ERROR_TYPE, fixed = TRUE)
})

################################################################################

options(opt.save)

################################################################################
