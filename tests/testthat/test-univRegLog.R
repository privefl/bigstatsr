################################################################################

context("UNIV_REG_LOG")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = FALSE)

TOL <- 1e-5

# Simulating some data
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
N <- nrow(mydata)
keep <- 2:4
M <- length(keep)
covar0 <- matrix(rnorm(N * 3), N)
lcovar <- list(NULL, covar0)
X0 <- as.matrix(mydata[, keep])
X0[, 1] <- X0[, 1] / 10
X0[, 2:3] <- X0[, 2:3] * 10
y <- mydata$admit

################################################################################

getGLM <- function(X, y, covar, ind = NULL) {
  res <- matrix(NA, M, 4)
  for (i in 1:M) {
    mod <- glm(y ~ cbind(X[, i, drop = FALSE], covar),
               family = "binomial", subset = ind,
               control = list(epsilon = 1e-10, maxit = 100))
    if (mod$converged)
      res[i, ] <- summary(mod)$coefficients[2, ]
  }
  res
}

################################################################################

test_that("equality with glm with all data", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(X0, type = t)
    for (covar in lcovar) {
      mod <- big_univRegLog(X, y, covar.train = covar)
      mat <- as.matrix(mod[, -3])
      dimnames(mat) <- NULL
      expect_equal(mat, getGLM(X, y, covar), tolerance = TOL)
    }
  }
})

################################################################################

ind <- sort(sample(N, N / 2))
while (mean(y[ind]) < 0.2 || mean(y[ind]) > 0.8) {
  ind <- sort(sample(N, N / 2))
}

test_that("equality with glm with only half the data", {
  for (t in ALL.TYPES) {
    X <- as.big.matrix(X0, type = t)
    for (covar in lcovar) {
      mod <- big_univRegLog(X, y[ind], covar.train = covar[ind, ],
                            ind.train = ind)
      mat <- as.matrix(mod[, -3])
      dimnames(mat) <- NULL
      expect_equal(mat, getGLM(X, y, covar, ind), tolerance = TOL)
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
