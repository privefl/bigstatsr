################################################################################

context("BIGXYT")

opt.save <- options(bigmemory.typecast.warning = FALSE)

# Simulating some data
N <- 50
M <- 100
x <- matrix(rnorm(N*M), N)
vec.center <- rnorm(M)
vec.scale <- rnorm(M)

################################################################################

test_that("equality with tcrossprod", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    test <- BigXYt(X = X,
                   block.size = 10,
                   vec.center = vec.center,
                   vec.scale = vec.scale)
    mat <- sweep(sweep(X[,], 2, vec.center, '-'), 2, vec.scale, '/')
    diff <- test[,] - tcrossprod(mat)
    expect_equal(max(abs(diff)), 0)
  }
})

################################################################################

test_that("equality with tcrossprod with use.Eigen=FALSE", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    test <- BigXYt(X = X,
                   block.size = 10,
                   vec.center = vec.center,
                   vec.scale = vec.scale,
                   use.Eigen = FALSE)
    mat <- sweep(sweep(X[,], 2, vec.center, '-'), 2, vec.scale, '/')
    diff <- test[,] - tcrossprod(mat)
    expect_equal(max(abs(diff)), 0)
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(N, N/2)
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    test <- BigXYt(X = X,
                   block.size = 10,
                   ind.train = ind,
                   vec.center = vec.center,
                   vec.scale = vec.scale)
    mat1 <- sweep(sweep(X[ind, ],  2, vec.center, '-'), 2, vec.scale, '/')
    mat2 <- sweep(sweep(X[-ind, ], 2, vec.center, '-'), 2, vec.scale, '/')
    diff1 <- test[[1]][,] - tcrossprod(mat1)
    diff2 <- test[[2]][,] - tcrossprod(mat2, mat1)
    expect_equal(max(abs(diff1)), 0)
    expect_equal(max(abs(diff2)), 0)
  }
})

################################################################################

options(opt.save)

################################################################################
