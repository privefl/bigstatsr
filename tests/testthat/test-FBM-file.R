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

test_that("option 'FBM.dir' works", {

  dir <- paste0(tempdir(), "_", basename(tempfile(pattern = "")))
  opt <- options(FBM.dir = dir)

  expect_message(X <- FBM(10, 10), "Creating directory")
  expect_identical(normalizePath(dirname(X$backingfile)), normalizePath(dir))

  X2 <- FBM.code256(10, 10)
  expect_identical(normalizePath(dirname(X2$backingfile)), normalizePath(dir))

  X3 <- as_FBM(matrix(1, 10, 10))
  expect_identical(normalizePath(dirname(X3$backingfile)), normalizePath(dir))

  options(opt)  # back to normal
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

test_that("add_columns() works", {

  init <- rnorm(100)
  X <- FBM(10, 10, init = init)
  expect_equal(file.size(X$bk), 10 * 10 * 8)
  X$add_columns(5)
  expect_equal(file.size(X$bk), 10 * 15 * 8)
  X[, 11:15] <- 5
  expect_equal(as.vector(X[, 1:10]), init)
  expect_true(all(X[, 11:15] == 5))

  X$is_read_only <- TRUE
  expect_error(X$add_columns(5), "This FBM is read-only.")
  X$is_read_only <- FALSE
  Sys.chmod(X$bk, "0444")  ## make it read-only
  if (file.access(X$bk, 2) != 0) {
    expect_error(X$add_columns(5),
                 "You don't have write permissions for this FBM.")
    expect_equal(dim(X), c(10, 15))
  }
})

################################################################################

test_that("add_columns() works with FBM.code256", {

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

  X$is_read_only <- TRUE
  expect_error(X$add_columns(5), "This FBM is read-only.")
  X$is_read_only <- FALSE
  Sys.chmod(X$bk, "0444")  ## make it read-only
  if (file.access(X$bk, 2) != 0) {
    expect_error(X$add_columns(5),
                 "You don't have write permissions for this FBM.")
    expect_equal(dim(X), c(10, 15))
  }
})

################################################################################

test_that("add_columns() works with large files", {

  skip_if_not(not_cran)
  skip_on_covr()

  system.time({
    X <- FBM(1e5, 1e4, type = "raw")
    X[, 1] <- 1
    expect_identical(file.size(X$backingfile), 1e9)
    X$add_columns(1e4)
    expect_identical(file.size(X$backingfile), 2e9)
    X$add_columns(1e4)
    X$add_columns(1e4)
    expect_identical(file.size(X$backingfile), 4e9)
    X$add_columns(1e4)
    # Problem was starting here before (no more appending)
    X$add_columns(1e4)
    expect_identical(file.size(X$backingfile), 6e9)
    expect_equal(dim(X), c(1e5, 6e4))
    expect_equal(X[, 1], rep(as.raw(1), nrow(X)))
  })
})

################################################################################
