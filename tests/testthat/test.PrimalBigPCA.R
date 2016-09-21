################################################################################

context("PRIMALBIGPCA")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

TOL <- 1e-3

# Simulating some data
N <- 100
M <- 50
s <- matrix(rnorm(N * M), N, M)
sigma <- crossprod(s)
x <- MASS::mvrnorm(N, mu = rep(0, M), Sigma = sigma) #matrix(rnorm(N*M), N)
vec.center <- rnorm(ncol(x))
vec.scale <- rnorm(ncol(x))

# function for comparing
diffPCs <- function(test, rot) {
  k <- ncol(test)
  diff1 <- 2 * abs(test - rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff2 <- 2 * abs(test + rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff <- pmin(diff1, diff2)
  max(diff)
}

################################################################################

test_that("equality with prcomp", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    mat <- sweep(sweep(X[,], 2, vec.center, '-'), 2, vec.scale, '/')
    for (k in list(NULL, 2, 10)) {
      test <- PrimalBigPCA(X = X,
                           block.size = 10,
                           vec.center = vec.center,
                           vec.scale = vec.scale,
                           k = k)
      pca <- prcomp(mat, center = FALSE, scale. = FALSE)
      expect_equal(diffPCs(test, pca$x), 0, tolerance = TOL)
    }
  }
})

################################################################################

test_that("equality with prcomp with half of the data", {
  ind <- sample(N, N/2)
  for (t in ALL.TYPES) {
    X <- as.big.matrix(x, type = t)
    mat1 <- sweep(sweep(X[ind, ],  2, vec.center, '-'), 2, vec.scale, '/')
    mat2 <- sweep(sweep(X[-ind, ], 2, vec.center, '-'), 2, vec.scale, '/')
    for (k in list(NULL, 2, 10)) {
      test <- PrimalBigPCA(X = X,
                           block.size = 10,
                           ind.train = ind,
                           vec.center = vec.center,
                           vec.scale = vec.scale,
                           k = k)
      pca <- prcomp(mat1, center = FALSE, scale. = FALSE)
      expect_equal(diffPCs(test[ind, ], pca$x), 0, tolerance = TOL)
      pred <- predict(pca, mat2)
      expect_equal(diffPCs(test[-ind, ], pred), 0, tolerance = TOL)
    }
  }
})

################################################################################

options(opt.save)

################################################################################
