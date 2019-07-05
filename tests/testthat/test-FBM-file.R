################################################################################

context("FBM_FILE")

################################################################################

test_that("$save() works", {
  X <- FBM(10, 10)
  expect_false(X$is_saved)
  X$save()
  expect_true(X$is_saved)
  X <- big_attach(X$rds)
  expect_true(X$is_saved)
})

################################################################################

test_that("sub_bk() works", {
  expect_identical(sub_bk("toto.bk"), "toto")
  expect_identical(sub_bk("toto.bk", ".rds"), "toto.rds")
  expect_error(sub_bk("toto.bk2"), "Path 'toto.bk2' must have 'bk' extension.")
  expect_error(sub_bk("toto.bk", "rds"), "extension starting with '.'")
  expect_identical(sub_bk("toto.bk", "rds", stop_if_not_ext = FALSE), "totords")
})

################################################################################

test_that("addColums() works", {
  init <- rnorm(100)
  X <- FBM(10, 10, init = init)
  expect_equal(file.size(X$bk), 800)
  X$add_columns(5)
  X[, 11:15] <- 5
  expect_equal(as.vector(X[, 1:10]), init)
  expect_true(all(X[, 11:15] == 5))
  expect_equal(file.size(X$bk), 10 * 15 * 8)
})

################################################################################

test_that("addColums() works with FBM.code256", {
  init <- sample(0:255, 100)
  X <- FBM.code256(10, 10, init = init, code = 0:255)
  expect_equal(file.size(X$bk), 100)
  X$add_columns(5)
  expect_s4_class(X, "FBM.code256")
  expect_equal(X$code256, 0:255)
  X[, 11:15] <- 5
  expect_equal(as.vector(X[, 1:10]), init)
  expect_true(all(X[, 11:15] == 5))
  expect_equal(file.size(X$bk), 10 * 15)
})

################################################################################
