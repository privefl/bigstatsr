################################################################################

context("SVD")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)

TOL <- 1e-5

# function for sampling scaling
sampleScale <- function() {
  tmp <- sample(list(c(TRUE, FALSE),
                     c(TRUE, TRUE),
                     c(FALSE, FALSE)))[[1]]
  list(center = tmp[1], scale = tmp[2])
}

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N*M, sd = 5), N)

###############################################################################

test_that("equality with prcomp", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))
    k <- sample(list(NULL, 2, 20), 1)[[1]] # NULL, 2 or 20
    sc <- sampleScale()

    test <- big_SVD(X.,
                    fun.scaling = big_scale(center = sc$center,
                                            scale = sc$scale),
                    k = k)
    pca <- prcomp(X[,], center = sc$center, scale. = sc$scale)
    expect_equal(diffPCs(predict(test), pca$x), 0, tolerance = TOL)
    expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)
  }
})

###############################################################################

test_that("equality with prcomp with half of the data", {
  ind <- sample(N, N/2)
  ind2 <- setdiff(1:N, ind)

  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    X. <- `if`(runif(1) > 0.5, X, bigmemory::describe(X))

    k <- sample(list(NULL, 2, 20), 1)[[1]] # NULL, 2 or 20
    sc <- sampleScale()

    test <- big_SVD(X.,
                    ind.row = ind,
                    fun.scaling = big_scale(center = sc$center,
                                            scale = sc$scale),
                    k = k)
    pca <- prcomp(X[ind, ], center = sc$center, scale. = sc$scale)

    expect_equal(diffPCs(predict(test), pca$x), 0, tolerance = TOL)
    expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)

    expect_equal(diffPCs(predict(test, X., ind.row = ind2),
                         predict(pca, X[ind2, ])), 0, tolerance = TOL)
  }
})

################################################################################

options(opt.save)

################################################################################
