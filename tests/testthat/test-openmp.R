################################################################################

context("OPENMP")
# Basically, test if any crash..

skip_if_not(not_cran)

################################################################################

test_that("parallel big_univLinReg() works", {

  G <- big_attachExtdata()
  rows <- sample(nrow(G), replace = TRUE)
  cols <- sample(ncol(G), replace = TRUE)
  n <- length(rows)
  y <- rnorm(n)
  covar <- matrix(rnorm(n * 3), n)

  test <- replicate(20, simplify = FALSE, {
    big_univLinReg(G, y, rows, cols, covar, ncores = 2)
  })
  true <- big_univLinReg(G, y, rows, cols, covar, ncores = 1)

  expect_true(all(sapply(test, all.equal, current = true)))
})

################################################################################
