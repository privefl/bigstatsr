################################################################################

context("APPLY")

set.seed(SEED)

################################################################################

# Simulating some data
N <- 73
M <- 4300
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)

################################################################################

test_that("equality with other functions", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    # get the means of each column
    colmeans <- big_apply(X, function(x, ind) colMeans(x[, ind]),
                          a.combine = "c", ncores = test_cores())
    expect_equal(colmeans, colMeans(X[]))

    # get the norms of each column
    colnorms <- big_apply(X, function(x, ind) sqrt(colSums(x[, ind]^2)),
                          a.combine = "c", ncores = test_cores())
    expect_equal(colnorms, sqrt(colSums(X[]^2)))

    # get the sums of each row
    rowsums <- big_apply(X, function(x, ind) rowSums(x[, ind]),
                         a.combine = "plus", ncores = test_cores())
    expect_equal(rowsums, rowSums(X[]))

    # get the maximum element of X (in absolute value)
    maxabs <- max(big_apply(X, function(x, ind) max(abs(x[, ind])),
                            a.combine = "c", ncores = test_cores()))
    expect_equal(maxabs, max(abs(X[])))

    # get the crossproduct between X and a matrix A
    A <- matrix(0, N, 10)
    A[] <- rnorm(length(A))
    expect_equal(big_cprodMat(X, A, ncores = test_cores()), crossprod(X[], A))

    # get the product between X and a matrix B
    B <- matrix(0, M, 10)
    B[] <- rnorm(length(B))
    expect_equal(big_prodMat(X, B, ncores = test_cores()), X[] %*% B)

    # no combine
    size <- sample(c(1, sample(M, size = 1), M), 1)
    no_comb <- big_apply(X, function(x, ind) colMeans(x[, ind, drop = FALSE]),
                         ncores = test_cores(), block.size = size)
    expect_equal(do.call(c, no_comb), colmeans)

    # big_parallelize()
    no_comb2 <- big_parallelize(X, function(x, ind) {
      colMeans(x[, ind, drop = FALSE])
    }, ncores = test_cores())
    expect_equal(unlist(no_comb2), colmeans)
  }
})

################################################################################
