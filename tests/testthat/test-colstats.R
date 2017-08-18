################################################################################

context("COLSTATS")

# Simulating some data
x <- matrix(rnorm(100, 100, 5), 10, 10)
ind <- 1:5

# variables to test
ALL.FUN <- c("sum", "var")

################################################################################

test_that("Equality with matrix operations", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    for (f in ALL.FUN) {
      eval(parse(text = sprintf(
        "expect_equal(big_colstats(X)$%s, apply(X[,], 2, %s))", f, f)))
      eval(parse(text = sprintf(
        "expect_equal(big_colstats(X, ind)$%s, apply(X[ind, ], 2, %s))", f, f)))
    }
  }
})

################################################################################

expect_error(big_colstats(big.matrix(10, 10, type = "raw")),
             GET_ERROR_TYPE(), fixed = TRUE)

################################################################################
