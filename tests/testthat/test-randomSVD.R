################################################################################

context("RANDOM_SVD")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)

TOL <- 1e-4

# function for comparing
diffPCs <- function(test, rot) {
  k <- ncol(test)
  diff1 <- 2 * abs(test - rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff2 <- 2 * abs(test + rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff <- pmin(diff1, diff2)
  mean(diff)
}

# Simulating some data
M <- 781
N <- 1501
x <- matrix(rnorm(N*M, sd = 5), N)

###############################################################################

test_that("equality with prcomp", {
  for (t in ALL.TYPES) {
    printf("\nTesting type %s\n", t)
    X <- as.big.matrix(x, type = t)
    for (k in c(2, 20)) {
      test <- big_randomSVD(X = X,
                            block.size = 100,
                            fun.scaling = big_noscale,
                            K = k)
      pca <- prcomp(X[,], center = FALSE, scale. = FALSE)
      expect_equal(diffPCs(test$u %*% diag(test$d), pca$x), 0, tolerance = TOL)
      expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)

      test <- big_randomSVD(X = X,
                            block.size = 100,
                            fun.scaling = big_center,
                            K = k)
      pca <- prcomp(X[,], center = TRUE, scale. = FALSE)
      expect_equal(diffPCs(test$u %*% diag(test$d), pca$x), 0, tolerance = TOL)
      expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)

      test <- big_randomSVD(X = X,
                            block.size = 100,
                            fun.scaling = big_scale,
                            K = k)
      pca <- prcomp(X[,], center = TRUE, scale. = TRUE)
      expect_equal(diffPCs(test$u %*% diag(test$d), pca$x), 0, tolerance = TOL)
      expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)
    }
  }
})

###############################################################################

test_that("equality with prcomp with half of the data", {
  ind <- sample(N, N/2)
  for (t in ALL.TYPES) {
    printf("\nTesting type %s\n", t)
    X <- as.big.matrix(x, type = t)
    for (k in c(2, 20)) {
      test <- big_randomSVD(X = X,
                            ind.train = ind,
                            block.size = 100,
                            fun.scaling = big_noscale,
                            K = k)
      pca <- prcomp(X[ind, ], center = FALSE, scale. = FALSE)
      expect_equal(diffPCs(test$u %*% diag(test$d), pca$x), 0, tolerance = TOL)
      expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)

      test <- big_randomSVD(X = X,
                            ind.train = ind,
                            block.size = 100,
                            fun.scaling = big_center,
                            K = k)
      pca <- prcomp(X[ind, ], center = TRUE, scale. = FALSE)
      expect_equal(diffPCs(test$u %*% diag(test$d), pca$x), 0, tolerance = TOL)
      expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)

      test <- big_randomSVD(X = X,
                            ind.train = ind,
                            block.size = 100,
                            fun.scaling = big_scale,
                            K = k)
      pca <- prcomp(X[ind, ], center = TRUE, scale. = TRUE)
      expect_equal(diffPCs(test$u %*% diag(test$d), pca$x), 0, tolerance = TOL)
      expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)
    }
  }
})

################################################################################

options(opt.save)

################################################################################
