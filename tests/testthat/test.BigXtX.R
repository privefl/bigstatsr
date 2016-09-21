################################################################################

context("BIGXTX")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
N <- 100
M <- 50
x <- matrix(rnorm(N*M), N)
vec.center <- rnorm(M)
vec.scale <- rnorm(M)

relDiff <- function(test, mat) {
  max(2 * abs(test - mat) / (abs(test) + abs(mat)))
}

################################################################################

test_that("equality with crossprod", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    mat <- sweep(sweep(X[,], 2, vec.center, '-'), 2, vec.scale, '/')
    test <- BigXtX(X = X,
                   block.size = 10,
                   vec.center = vec.center,
                   vec.scale = vec.scale)
    expect_equal(relDiff(test[,], crossprod(mat)), 0)
  }
})

################################################################################

test_that("equality with crossprod with half of the data", {
  ind <- sample(N, N/2)
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    mat <- sweep(sweep(X[ind, ],  2, vec.center, '-'), 2, vec.scale, '/')
    test <- BigXtX(X = X,
                   block.size = 10,
                   ind.train = ind,
                   vec.center = vec.center,
                   vec.scale = vec.scale)
    expect_equal(relDiff(test[,], crossprod(mat)), 0)
  }
})

################################################################################

options(opt.save)

################################################################################
