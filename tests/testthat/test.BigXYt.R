################################################################################

context("BIGXYT")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
N <- 50
M <- 100
x <- matrix(rnorm(N*M), N)
vec.center <- rnorm(M)
vec.scale <- rnorm(M)

relDiff <- function(test, mat) {
  max(2 * abs(test - mat) / (abs(test) + abs(mat)))
}

################################################################################

test_that("equality with tcrossprod", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    mat <- sweep(sweep(X[,], 2, vec.center, '-'), 2, vec.scale, '/')
    for (use in c(TRUE, FALSE)) {
      test <- BigXYt(X = X,
                     block.size = 10,
                     vec.center = vec.center,
                     vec.scale = vec.scale,
                     use.Eigen = use)
      expect_equal(relDiff(test[,], tcrossprod(mat)), 0)
    }
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(N, N/2)
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    mat1 <- sweep(sweep(X[ind, ],  2, vec.center, '-'), 2, vec.scale, '/')
    mat2 <- sweep(sweep(X[-ind, ], 2, vec.center, '-'), 2, vec.scale, '/')
    for (use in c(TRUE, FALSE)) {
      test <- BigXYt(X = X,
                     block.size = 10,
                     ind.train = ind,
                     vec.center = vec.center,
                     vec.scale = vec.scale,
                     use.Eigen = use)
      expect_equal(relDiff(test[[1]][,], tcrossprod(mat1)), 0)
      expect_equal(relDiff(test[[2]][,], tcrossprod(mat2, mat1)), 0)
    }
  }
})

################################################################################

options(opt.save)

################################################################################
