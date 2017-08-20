################################################################################

context("BM_CODE_ACC")

x <- matrix(rnorm(256, mean = 100, sd = 10), 16)

X <- big_copy(x, type = "raw")
X[] <- as.raw(0:255)
X2 <- add_code256(X, code = as.vector(x))
expect_s4_class(X2, "FBM")
expect_s4_class(X2, "FBM.code256")

################################################################################

test_that("same dimensions", {
  expect_equal(nrow(X2), nrow(x))
  expect_equal(ncol(X2), ncol(x))
  expect_equal(dim(X2), dim(x))
  expect_equal(length(X2), length(x))
})

################################################################################

test_that("same accessing", {

  expect_equal(X2[], x[])
  expect_equal(X2[], x[])
  expect_equal(X2[, , drop = FALSE], x[, , drop = FALSE])
  expect_equal(X2[, , drop = TRUE], x[, , drop = TRUE])
  expect_equal(X2[1, ], x[1, ])
  expect_equal(X2[1, , drop = FALSE], x[1, , drop = FALSE])
  expect_equal(X2[1, , drop = TRUE], x[1, , drop = TRUE])
  expect_equal(X2[cbind(1:5, 1:5)], x[cbind(1:5, 1:5)])

  for (ind in list(1:5, -(1:5), c(TRUE, FALSE, TRUE))) {

    expect_equal(X2[ind, ], x[ind, ])
    expect_equal(X2[ind, , drop = FALSE], x[ind, , drop = FALSE])
    expect_equal(X2[ind, , drop = TRUE], x[ind, , drop = TRUE])
    expect_equal(X2[, 1], x[, 1])
    expect_equal(X2[, 1, drop = FALSE], x[, 1, drop = FALSE])
    expect_equal(X2[, 1, drop = TRUE], x[, 1, drop = TRUE])
    expect_equal(X2[1, 1], x[1, 1])
    expect_equal(X2[1, 1, drop = FALSE], x[1, 1, drop = FALSE])
    expect_equal(X2[1, 1, drop = TRUE], x[1, 1, drop = TRUE])
    expect_equal(X2[ind, 1], x[ind, 1])
    expect_equal(X2[ind, 1, drop = FALSE], x[ind, 1, drop = FALSE])
    expect_equal(X2[ind, 1, drop = TRUE], x[ind, 1, drop = TRUE])
    expect_equal(X2[, ind], x[, ind])
    expect_equal(X2[, ind, drop = FALSE], x[, ind, drop = FALSE])
    expect_equal(X2[, ind, drop = TRUE], x[, ind, drop = TRUE])
    expect_equal(X2[1, ind], x[1, ind])
    expect_equal(X2[1, ind, drop = FALSE], x[1, ind, drop = FALSE])
    expect_equal(X2[1, ind, drop = TRUE], x[1, ind, drop = TRUE])
    expect_equal(X2[ind, ind], x[ind, ind])
    expect_equal(X2[ind, ind, drop = FALSE], x[ind, ind, drop = FALSE])
    expect_equal(X2[ind, ind, drop = TRUE], x[ind, ind, drop = TRUE])
  }
})

################################################################################
