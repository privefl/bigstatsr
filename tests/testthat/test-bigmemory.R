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
}

################################################################################
