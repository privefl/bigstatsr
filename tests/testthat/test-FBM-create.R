################################################################################

context("CREATE")

set.seed(SEED)

################################################################################

x0 <- matrix(rnorm(256, mean = 100, sd = 10), 16)

options(bigstatsr.downcast.warning = FALSE)

################################################################################

test_that("Creating an FBM works", {

  for (t in c(TEST.TYPES, "FBM.code256")) {

    is_ro <- sample(c(TRUE, FALSE), 1)

    if (t == "FBM.code256") {

      X0 <- FBM(nrow(x0), ncol(x0), type = "raw", init = x0, is_read_only = is_ro)
      expect_identical(X0$is_read_only, is_ro)
      if (is_ro) expect_error(X0[1] <- 1, "This FBM is read-only.")
      X0$is_read_only <- FALSE
      X0[] <- as.raw(0:255)
      X0$is_read_only <- is_ro

      X <- add_code256(X0, code = as.vector(x0))
      expect_identical(X$is_read_only, is_ro)
      expect_s4_class(X, "FBM")
      expect_s4_class(X, "FBM.code256")

      expect_error(FBM(nrow(x0), ncol(x0) - 1, type = "raw",
                       backingfile = sub_bk(X$bk), create_bk = FALSE),
                   "Inconsistency between size of backingfile and dimensions.")
      expect_error(FBM(nrow(x0), ncol(x0), type = "raw", backingfile = sub_bk(X$bk),
                       create_bk = FALSE, init = x0),
                   "You can't use `init` when using `create_bk = FALSE`.")
      X2 <- FBM(nrow(x0), ncol(x0), type = "raw", backingfile = sub_bk(X$bk),
                create_bk = FALSE, is_read_only = is_ro)
      expect_s4_class(X2, "FBM")
      expect_true(X2$backingfile == X$backingfile)
      expect_equal(X2$is_read_only, is_ro)
      if (is_ro) expect_error(X3[1] <- 1, "This FBM is read-only.")

      X3 <- X$as.FBM()
      expect_equal(X3[], X2[])
      expect_equal(X3$is_read_only, is_ro)
      if (is_ro) expect_error(X3[1] <- 1, "This FBM is read-only.")

      X4 <- as_FBM(x0, type = "raw", is_read_only = is_ro)
      expect_equal(X4[], matrix(as.raw(x0), 16))
      expect_equal(X4$is_read_only, is_ro)
      if (is_ro) expect_error(X4[1] <- 1, "This FBM is read-only.")

    } else {

      X <- as_FBM(x0, type = t, is_read_only = is_ro)
      expect_s4_class(X, "FBM")
      expect_equal(X$is_read_only, is_ro)
      if (is_ro) expect_error(X[1] <- 1, "This FBM is read-only.")

      X2 <- FBM(nrow(x0), ncol(x0), type = t, init = x0, is_read_only = !is_ro)
      expect_s4_class(X2, "FBM")
      expect_true(typeof(X2) == t)
      expect_equal(X2[], X[])
      expect_equal(X2$is_read_only, !is_ro)

      expect_error(FBM(nrow(x0), ncol(x0) - 1, type = t,
                       backingfile = sub_bk(X$bk), create_bk = FALSE),
                   "Inconsistency between size of backingfile and dimensions.")
      expect_error(FBM(nrow(x0), ncol(x0), type = t, init = x0,
                       backingfile = sub_bk(X$bk), create_bk = FALSE),
                   "You can't use `init` when using `create_bk = FALSE`.")
      X3 <- FBM(nrow(x0), ncol(x0), type = t,
                backingfile = sub_bk(X$bk), create_bk = FALSE)
      expect_s4_class(X3, "FBM")
      expect_true(typeof(X3) == t)
      expect_equal(X3[], X[])
      expect_equal(X3$is_read_only, FALSE)

    }

  }
})

################################################################################

test_that("big_attach() works (also with previous versions)", {

  X <- big_attachExtdata()
  expect_s4_class(X, "FBM.code256")
  expect_false(X$is_read_only)

  old_file <- system.file("testdata", "before_readonly.rds", package = "bigstatsr")
  expect_message(X <- big_attach(old_file),
                 "[FBM from an old version? Reconstructing..|You should `$save()` it again.]")
  expect_s4_class(X, "FBM.code256")
  expect_false(X$is_read_only)
})

################################################################################
