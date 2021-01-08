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

# test_that("parallel big_univLogReg() works", {
#
#   G <- big_attachExtdata()
#   rows <- sample(nrow(G), replace = TRUE)
#   cols <- sample(ncol(G), replace = TRUE)
#   n <- length(rows)
#   y <- sample(0:1, size = n, replace = TRUE)
#   covar <- matrix(rnorm(n * 3), n)
#
#   test <- replicate(20, simplify = FALSE, {
#     big_univLogReg(G, y, rows, cols, covar, ncores = 2)
#   })
#   true <- big_univLogReg(G, y, rows, cols, covar, ncores = 1)
#
#   expect_true(all(sapply(test, all.equal, current = true)))
# })

################################################################################

test_that("parallel big_prodVec() works", {

  G <- big_attachExtdata()
  rows <- sample(nrow(G), nrow(G) - sample(0:3, 1), replace = TRUE)
  cols <- sample(ncol(G), ncol(G) - sample(0:3, 1), replace = TRUE)
  y <- rnorm(length(cols))

  test <- replicate(20, simplify = FALSE, {
    big_prodVec(G, y, rows, cols, ncores = 2)
  })
  true <- big_prodVec(G, y, rows, cols, ncores = 1)

  expect_true(all(sapply(test, all.equal, current = true)))
})

################################################################################

test_that("parallel big_cprodVec() works", {

  G <- big_attachExtdata()
  rows <- sample(nrow(G), nrow(G) - sample(0:3, 1), replace = TRUE)
  cols <- sample(ncol(G), ncol(G) - sample(0:3, 1), replace = TRUE)
  y <- rnorm(length(rows))

  test <- replicate(20, simplify = FALSE, {
    big_cprodVec(G, y, rows, cols, ncores = 2)
  })
  true <- big_cprodVec(G, y, rows, cols, ncores = 1)

  expect_true(all(sapply(test, all.equal, current = true)))
})

################################################################################

test_that("parallel big_colstats() works", {

  G <- big_attachExtdata()
  rows <- sample(nrow(G), replace = TRUE)
  cols <- sample(ncol(G), replace = TRUE)

  test <- replicate(20, simplify = FALSE, {
    big_colstats(G, rows, cols, ncores = 2)
  })
  true <- big_colstats(G, rows, cols, ncores = 1)

  expect_true(all(sapply(test, all.equal, current = true)))
})

################################################################################
