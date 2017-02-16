################################################################################

context("COLSTATS")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
x <- matrix(rnorm(100, 0, 5), 10, 10)
ind <- 1:5

# variables to test
ALL.FUN <- c("sum", "var")

################################################################################

test_that("Equality with matrix operations", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)

    for (f in ALL.FUN) {
      eval(parse(text = sprintf(
        "expect_equal(big_colstats(X)$%s, apply(X[,], 2, %s))", f, f)))
      eval(parse(text = sprintf(
        "expect_equal(big_colstats(X, ind)$%s, apply(X[ind, ], 2, %s))", f, f)))
    }
  }
})

################################################################################

options(opt.save)

################################################################################
