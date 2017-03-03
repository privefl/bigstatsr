################################################################################

context("APPLY")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)


# Simulating some data
N <- 73
M <- 4300
x <- matrix(rnorm(N*M, sd = 5), N)

###############################################################################

test_that("equality with other functions", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    # get the means of each column
    colmeans <- big_apply(X., function(x, ind) colMeans(x[, ind]),
                          a.combine = 'c')
    expect_equal(colmeans, colMeans(X[,]))

    # get the norms of each column
    colnorms <- big_apply(X., function(x, ind) sqrt(colSums(x[, ind]^2)),
                          a.combine = 'c')
    expect_equal(colnorms, sqrt(colSums(X[,]^2)))

    # get the sums of each row
    rowsums <- big_apply(X., function(x, ind) rowSums(x[, ind]), a.combine = '+')
    expect_equal(rowsums, rowSums(X[,]))

    # get the maximum element of X (in absolute value)
    maxabs <- max(big_apply(X., function(x, ind) max(abs(x[, ind])),
                            a.combine = 'c'))
    expect_equal(maxabs, max(abs(X[,])))

    # get the crossproduct between X and a matrix A
    A <- matrix(0, N, 10)
    A[] <- rnorm(length(A))
    expect_equal(big_cprodMat(X., A), crossprod(X[,], A))

    # get the product between X and a matrix B
    B <- matrix(0, M, 10)
    B[] <- rnorm(length(B))
    expect_equal(big_prodMat(X., B), X[,] %*% B)
  }
})

################################################################################

options(opt.save)

################################################################################
