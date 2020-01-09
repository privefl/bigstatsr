################################################################################

context("BIGMEMORY")

################################################################################

for (t in c("raw", "integer", "float", "double")) {

  X <- FBM(10, 10, type = t)
  if (t != "raw") X[1] <- NA

  # Conversion works
  expect_s4_class(X$bm.desc(), "big.matrix.descriptor")
  expect_s4_class(X2 <- X$bm(), "big.matrix")
  expect_identical(typeof(X2), typeof(X))
  if (t != "float") expect_equal(as.numeric(X2[1]), as.numeric(X[1]))

  # Product works with BM and FBM (of type 'double')
  library(bigalgebra)
  A <- matrix(1, 10, 10)
  if (t == "double") {
    expect_is(X %*% A, "matrix")
    expect_s4_class(X2 %*% A, "big.matrix")
  }

  # Permissions
  X$is_read_only <- TRUE
  expect_warning(X2 <- X$bm(), "This FBM is supposed to be read-only")
  X2[1] <- 3
  expect_equal(X2[1], 3)
  X$is_read_only <- FALSE
  Sys.chmod(X$bk, "0444")  ## make it read-only
  expect_warning(X3 <- X$bm(), "big.matrix object could only be opened read-only.")
  # X3[1] <- 4  ## would crash the session
}

################################################################################
