################################################################################

context("COPY")

set.seed(SEED)

################################################################################

x0 <- matrix(rnorm(256, mean = 100, sd = 10), 16)

options(bigstatsr.downcast.warning = FALSE)

################################################################################

test_that("Copy of same type", {

  for (t in c(TEST.TYPES, "FBM.code256")) {

    is_ro <- sample(c(TRUE, FALSE), 1)

    if (t == "FBM.code256") {

      X <- big_copy(x0, type = "raw", is_read_only = is_ro)
      expect_identical(X$is_read_only, is_ro)
      X$is_read_only <- FALSE
      X[] <- as.raw(0:255)
      X$is_read_only <- is_ro
      X <- add_code256(X, code = as.vector(x0))
      expect_identical(X$is_read_only, is_ro)
      expect_s4_class(X, "FBM")
      expect_s4_class(X, "FBM.code256")

      X2 <- big_copy(X, is_read_only = !is_ro)
      expect_s4_class(X2, "FBM.code256")
      expect_false(X2$backingfile == X$backingfile)
      expect_equal(X2[], X[])
      expect_equal(X2$is_read_only, !is_ro)

      ind <- sample(16, 10)
      X3 <- big_copy(X, ind.row = ind, ind.col = ind)
      expect_s4_class(X3, "FBM.code256")
      expect_false(X3$backingfile == X$backingfile)
      expect_equal(X3[], X[ind, ind])
      expect_equal(X3$is_read_only, FALSE)

      X4 <- big_copy(X, type = "double")
      expect_s4_class(X4, "FBM")
      expect_failure(expect_s4_class(X4, "FBM.code256"))
      expect_false(X4$backingfile == X$backingfile)
      expect_equal(X4[], X[])

    } else {

      X <- big_copy(x0, type = t, is_read_only = is_ro)
      expect_s4_class(X, "FBM")
      expect_equal(X$is_read_only, is_ro)

      X2 <- big_copy(X, is_read_only = !is_ro)
      expect_s4_class(X2, "FBM")
      expect_false(X2$backingfile == X$backingfile)
      expect_true(typeof(X2) == t)
      expect_equal(X2[], X[])
      expect_equal(X2$is_read_only, !is_ro)

      ind <- sample(16, 10)
      X3 <- big_copy(X, ind.row = ind, ind.col = ind)
      expect_s4_class(X3, "FBM")
      expect_false(X3$backingfile == X$backingfile)
      expect_true(typeof(X3) == t)
      expect_equal(X3[], X[ind, ind])
      expect_equal(X3$is_read_only, FALSE)
    }

  }
})

################################################################################

test_that("Copy with conversion", {

  skip_if_not(not_cran)

  options(bigstatsr.downcast.warning = TRUE)

  X <- big_copy(x0)
  expect_identical(X[1, 1] <- NA, NA)
  expect_identical(X[1, 1], NA_real_)
  expect_warning(X2 <- big_copy(X, type = "integer"))
  expect_identical(X2[1, 1], NA_integer_)
  expect_equal(X2[], floor(X[]))

  options(bigstatsr.downcast.warning = FALSE)
})

################################################################################

test_that("Copy from big.matrix", {
  skip_if_not_installed("bigmemory")
  X <- bigmemory::big.matrix(10, 10, shared = FALSE)
  X2 <- big_copy(X)
  expect_equal(X2[], X[])
  X[] <- rnorm(length(X))
  expect_true(all(is.na(X2[])))
  X3 <- big_copy(X)
  expect_equal(X3[], X[])
})

################################################################################

test_that("option 'FBM.dir' works", {

  X <- FBM(10, 10)

  dir <- paste0(tempdir(), "_", basename(tempfile(pattern = "")))
  opt <- options(FBM.dir = dir)

  expect_message(X2 <- big_copy(X), "Creating directory")
  expect_identical(normalizePath(dirname(X2$backingfile)), dir)

  options(opt)  # back to normal
})

################################################################################
