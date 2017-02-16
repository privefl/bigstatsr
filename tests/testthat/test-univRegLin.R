################################################################################

context("UNIV_REG_LIN")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

# Simulating some data
data("trees")
N <- nrow(trees)
covar0 <- matrix(rnorm(N * 3), N)
lcovar <- list(NULL, covar0)
X0 <- as.matrix(trees[, -1])
y <- trees[, 1]

################################################################################

test_that("equality with lm with all data", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(X0, type = t)
    for (covar in lcovar) {
      lm1 <- summary(lm(y ~ cbind(X[, 1, drop = FALSE], covar)))
      lm2 <- summary(lm(y ~ cbind(X[, 2, drop = FALSE], covar)))
      expect_equivalent(as.matrix(big_univRegLin(X, y, covar = covar)),
                        rbind(lm1$coefficients[2, ],
                              lm2$coefficients[2, ]))
    }
  }
})

################################################################################

test_that("equality with lm with only half the data", {
  ind <- sample(N, N / 2)

  for (t in ALL.TYPES) {
    X <- as.big.matrix(X0, type = t)
    for (covar in lcovar) {
      lm1 <- summary(lm(y ~ cbind(X[, 1, drop = FALSE], covar), subset = ind))
      lm2 <- summary(lm(y ~ cbind(X[, 2, drop = FALSE], covar), subset = ind))
      expect_equivalent(as.matrix(big_univRegLin(X, y[ind],
                                                 covar = covar[ind, ],
                                                 ind.train = ind)),
                        rbind(lm1$coefficients[2, ], lm2$coefficients[2, ]))
    }
  }
})

################################################################################

# test_that("Expect error from unknown type", {
#   x <- as.raw(sample(0:255, 100))
#   X <- as.big.matrix(matrix(x), type = "raw")
#   expect_error(big_univRegLin(X, y), ERROR_TYPE, fixed = TRUE)
# })

################################################################################

options(opt.save)

################################################################################
