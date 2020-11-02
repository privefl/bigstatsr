################################################################################

context("TRANSPOSE")

set.seed(SEED)

options(bigstatsr.downcast.warning = FALSE)

################################################################################

# Simulating some data
x <- matrix(rnorm(2e4, 100, 5), 200, 100)

################################################################################

test_that("Equality with t()", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    test <- big_transpose(X)
    expect_identical(test[], t(X[]))
  }
})

################################################################################
