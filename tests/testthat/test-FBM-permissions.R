################################################################################

context("PERMISSIONS")

################################################################################

tmp <- tempfile()
X <- FBM(10, 10, backingfile = tmp, init = NA)$save()
expect_output(print(X), "A Filebacked Big Matrix of type 'double'")

expect_identical(file.access(X$bk, 4), setNames(0L, X$bk))
expect_identical(file.access(X$bk, 2), setNames(0L, X$bk))
X <- big_attach(paste0(tmp, ".rds"))
expect_true(all(is.na(X[])))
X[] <- 1
expect_true(all(X[] == 1))

X$is_read_only <- TRUE
expect_output(print(X), "A read-only Filebacked Big Matrix of type 'double'")
expect_error(X[] <- 2, "This FBM is read-only.")
expect_true(all(X[] == 1))

Sys.chmod(paste0(tmp, ".bk"), "0444")  ## make it read-only
if (file.access(X$bk, 2) != 0) {
  X <- big_attach(paste0(tmp, ".rds"))
  expect_true(all(X[] == 1))
  expect_error(X[] <- 3, "You don't have write permissions for this FBM.")
  Sys.chmod(paste0(tmp, ".bk"), "0666")  ## make it writable again
  X[] <- 4
  expect_true(all(X[] == 4))
}

################################################################################
