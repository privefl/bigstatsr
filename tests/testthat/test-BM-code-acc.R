################################################################################

context("BM_CODE_ACC")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

x <- matrix(rnorm(256, sd = 10), 16)

X <- big.matrix(nrow(x), ncol(x), type = "raw")
X[] <- as.raw(0:255)
X2 <- as.BM.code(X, code = as.vector(x))
expect_s4_class(X2, "BM.code")

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
  expect_equal(X2[1:5, ], x[1:5, ])
  expect_equal(X2[1:5, , drop = FALSE], x[1:5, , drop = FALSE])
  expect_equal(X2[1:5, , drop = TRUE], x[1:5, , drop = TRUE])
  expect_equal(X2[, 1], x[, 1])
  expect_equal(X2[, 1, drop = FALSE], x[, 1, drop = FALSE])
  expect_equal(X2[, 1, drop = TRUE], x[, 1, drop = TRUE])
  expect_equal(X2[1, 1], x[1, 1])
  expect_equal(X2[1, 1, drop = FALSE], x[1, 1, drop = FALSE])
  expect_equal(X2[1, 1, drop = TRUE], x[1, 1, drop = TRUE])
  expect_equal(X2[1:5, 1], x[1:5, 1])
  expect_equal(X2[1:5, 1, drop = FALSE], x[1:5, 1, drop = FALSE])
  expect_equal(X2[1:5, 1, drop = TRUE], x[1:5, 1, drop = TRUE])
  expect_equal(X2[, 1:5], x[, 1:5])
  expect_equal(X2[, 1:5, drop = FALSE], x[, 1:5, drop = FALSE])
  expect_equal(X2[, 1:5, drop = TRUE], x[, 1:5, drop = TRUE])
  expect_equal(X2[1, 1:5], x[1, 1:5])
  expect_equal(X2[1, 1:5, drop = FALSE], x[1, 1:5, drop = FALSE])
  expect_equal(X2[1, 1:5, drop = TRUE], x[1, 1:5, drop = TRUE])
  expect_equal(X2[1:5, 1:5], x[1:5, 1:5])
  expect_equal(X2[1:5, 1:5, drop = FALSE], x[1:5, 1:5, drop = FALSE])
  expect_equal(X2[1:5, 1:5, drop = TRUE], x[1:5, 1:5, drop = TRUE])
  expect_equal(X2[cbind(1:5, 1:5)], x[cbind(1:5, 1:5)])
})

################################################################################

options(opt.save)

################################################################################
