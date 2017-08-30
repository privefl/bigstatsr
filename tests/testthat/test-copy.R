################################################################################

context("COPY")

x0 <- matrix(rnorm(256, mean = 100, sd = 10), 16)

################################################################################

test_that("Copy of same type", {

  for (t in c(TEST.TYPES, "FBM.code256")) {

    if (t == "FBM.code256") {
      X <- big_copy(x0, type = "raw")
      X[] <- as.raw(0:255)
      X <- add_code256(X, code = as.vector(x0))
      expect_s4_class(X, "FBM")
      expect_s4_class(X, "FBM.code256")

      x <- x0

      X2 <- big_copy(X)
      expect_s4_class(X2, "FBM.code256")
      expect_false(X2$backingfile == X$backingfile)
      expect_equal(X2[], X[])

      ind <- sample(16, 10)
      X3 <- big_copy(X, ind.row = ind, ind.col = ind)
      expect_s4_class(X3, "FBM.code256")
      expect_false(X3$backingfile == X$backingfile)
      expect_equal(X3[], X[ind, ind])
    } else {
      X <- big_copy(x0, type = t)
      expect_s4_class(X, "FBM")

      x <- X[]

      X2 <- big_copy(X)
      expect_s4_class(X2, "FBM")
      expect_false(X2$backingfile == X$backingfile)
      expect_true(typeof(X2) == t)
      expect_equal(X2[], X[])

      ind <- sample(16, 10)
      X3 <- big_copy(X, ind.row = ind, ind.col = ind)
      expect_s4_class(X3, "FBM")
      expect_false(X3$backingfile == X$backingfile)
      expect_true(typeof(X3) == t)
      expect_equal(X3[], X[ind, ind])
    }

  }
})

################################################################################

test_that("Copy with conversion", {
  options(bigstatsr.typecast.warning = TRUE)

  X <- big_copy(x0)
  expect_warning(X[1, 1] <- NA)
  expect_identical(X[1, 1], NA_real_)
  expect_warning(X2 <- big_copy(X, type = "integer"))
  expect_identical(X2[1, 1], NA_integer_)
  expect_equal(X2[], floor(X[]))

  options(bigstatsr.typecast.warning = FALSE)
})

################################################################################

test_that("Copy from big.matrix", {
  X <- bigmemory::big.matrix(10, 10, shared = FALSE)
  X2 <- big_copy(X)
  expect_equal(X2[], X[])
  X[] <- rnorm(length(X))
  expect_true(all(is.na(X2[])))
  X3 <- big_copy(X)
  expect_equal(X3[], X[])
})

################################################################################
