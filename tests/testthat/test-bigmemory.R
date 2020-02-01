################################################################################

context("BIGMEMORY")

################################################################################

test_that("Conversion to big.matrix works", {

  skip_if_not_installed("bigmemory")

  for (t in c("raw", "integer", "float", "double")) {

    X <- FBM(10, 10, type = t, init = 0)
    if (t != "raw") X[1] <- NA

    # Conversion works
    expect_s4_class(X$bm.desc(), "big.matrix.descriptor")
    expect_s4_class(X2 <- X$bm(), "big.matrix")
    expect_identical(typeof(X2), typeof(X))
    if (t != "float" && not_cran)
      expect_equal(as.numeric(X2[1]), as.numeric(X[1]))

    # Permissions
    X$is_read_only <- TRUE
    expect_warning(X2 <- X$bm(), "This FBM is supposed to be read-only")
  }

})

################################################################################
