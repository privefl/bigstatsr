################################################################################

context("SVD")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

TOL <- 1e-6

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

    k <- sample(list(NULL, 2, 20), 1)[[1]] # NULL, 2 or 20
    sc <- sampleScale()

    test <- big_SVD(X = X,
                    fun.scaling = big_scale(center = sc$center,
                                            scale = sc$scale),
                    k = k, use.Eigen = (runif(1) > 0.5))
    pca <- prcomp(X[,], center = sc$center, scale. = sc$scale)
    expect_equal(diffPCs(test$u %*% diag(test$d), pca$x), 0, tolerance = TOL)
    expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)
  }
})

###############################################################################

test_that("equality with prcomp with half of the data", {
  ind <- sample(N, N/2)
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)

    k <- sample(list(NULL, 2, 20), 1)[[1]] # NULL, 2 or 20
    sc <- sampleScale()

    test <- big_SVD(X = X,
                    ind.train = ind,
                    fun.scaling = big_scale(center = sc$center,
                                            scale = sc$scale),
                    k = k, use.Eigen = (runif(1) > 0.5))
    pca <- prcomp(X[ind, ], center = sc$center, scale. = sc$scale)
    expect_equal(diffPCs(test$u %*% diag(test$d), pca$x), 0, tolerance = TOL)
    expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)
  }
})

################################################################################

options(opt.save)

################################################################################
