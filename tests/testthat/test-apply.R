################################################################################

context("APPLY")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)


# Simulating some data
N <- 73
M <- 4300
x <- matrix(rnorm(N*M, sd = 5), N)

###############################################################################

test_that("equality with other functions", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)

    # get the means of each column
    colmeans <- big_apply(X, colMeans, .combine = 'c')
    expect_equal(colmeans, colMeans(X[,]))

    # get the norms of each column
    colnorms <- big_apply(X, function(mat) sqrt(colSums(mat^2)),
                          .combine = 'c')
    expect_equal(colnorms, sqrt(colSums(X[,]^2)))

    # get the sums of each row
    rowsums <- big_apply(X, rowSums, .combine = '+')
    expect_equal(rowsums, rowSums(X[,]))

    # get the maximum element of X (in absolute value)
    maxabs <- max(big_apply(X, function(x) max(abs(x)), .combine = 'c'))
    expect_equal(maxabs, max(abs(X[,])))

    # get the crossproduct between X and a matrix A
    A <- matrix(0, N, 10)
    A[] <- rnorm(length(A))
    XtA <- big_apply(X, function(x) crossprod(x, A), .combine = 'rbind')
    expect_equal(XtA, crossprod(X[,], A))

    # get the product between X and a matrix B
    B <- matrix(0, M, 10)
    B[] <- rnorm(length(B))
    XB <- big_apply(X, function(x, ind, A) {
      x.part <- x[, ind, drop = FALSE]
      A.part <- A[ind, , drop = FALSE]
      x.part %*% A.part
    }, .combine = '+', ind.arg = TRUE, A = B)
    expect_equal(XB, X[,] %*% B)
  }
})

################################################################################

options(opt.save)

################################################################################
