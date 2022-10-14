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

test_that("option 'FBM.dir' works", {

  X <- FBM(10, 10)

  dir <- tempfile(pattern = "tmpdir")
  opt <- options(FBM.dir = dir)

  expect_message(X2 <- big_transpose(X), "Creating directory")
  expect_identical(normalizePath(dirname(X2$backingfile)), normalizePath(dir))

  options(opt)  # back to normal
})

################################################################################
