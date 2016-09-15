################################################################################

context("COLSTATS")

opt.save <- options(bigmemory.typecast.warning = FALSE)

# Simulating some data
x <- matrix(rnorm(100, 0, 5), 10, 10)
ind <- 1:5

# variables to test
ALL.TYPES <- c("char", "short", "integer", "float", "double")
ALL.FUN <- c("sum", "mean", "sd", "var")

################################################################################

test_that("Equality with matrix operations", {
  for (t in ALL.TYPES) {
    printf("\nTesting type %s\n", t)
    X <- big.matrix(10, 10, type = t)
    X[] <- x
    for (f in ALL.FUN) {
      eval(parse(text = sprintf(
        "expect_equal(apply(X[,], 2, %s), col%ss(X))", f, f)))
      eval(parse(text = sprintf(
        "expect_equal(apply(X[ind, ], 2, %s), col%ss(X, ind))", f, f)))
    }
  }
})

################################################################################

options(opt.save)

################################################################################
