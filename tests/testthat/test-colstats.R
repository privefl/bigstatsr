################################################################################

context("COLSTATS")

set.seed(SEED)

################################################################################

# Simulating some data
x <- matrix(rnorm(100, 100, 5), 10, 10)
ind <- 1:5

################################################################################

test_that("Equality with matrix operations", {

  for (t in TEST.TYPES) {

    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    expect_identical(big_colstats(X, ind.row = integer()),
                     data.frame(sum = rep(0, ncol(X)), var = NaN))
    expect_identical(big_colstats(X, ind.col = integer()),
                     data.frame(sum = numeric(), var = numeric()))
    expect_error(big_colstats(X, ind.row = NULL), "'ind.row' can't be `NULL`.")
    expect_error(big_colstats(X, ind.col = NULL), "'ind.col' can't be `NULL`.")

    for (f in c("sum", "var")) {
      eval(parse(text = sprintf(
        "expect_equal(big_colstats(X)$%s, apply(X[,], 2, %s))", f, f)))
      eval(parse(text = sprintf(
        "expect_equal(big_colstats(X, ind)$%s, apply(X[ind, ], 2, %s))", f, f)))
      eval(parse(text = sprintf(
        "expect_equal(big_colstats(X, ind, ncores = 2)$%s, apply(X[ind, ], 2, %s))", f, f)))
    }
  }
})

################################################################################
