################################################################################

context("TRANSPOSE")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)

# Simulating some data
x <- matrix(rnorm(2e4, 0, 5), 200, 100)

################################################################################

test_that("Equality with t()", {
  for (t in ALL.TYPES) {
    X <- `if`(t == "raw", asBMcode(x), as.big.matrix(x, type = t))
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    test <- big_transpose(X., BM(descriptor = FALSE))
    expect_identical(test[,], t(X[,]))
  }
})

################################################################################

options(opt.save)

################################################################################
