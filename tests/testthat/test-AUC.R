################################################################################

context("AUC")

N <- 100
x0 <- rnorm(N, mean = runif(1))
x1 <- rnorm(N, mean = 2 * runif(1))
x <- c(x0, x1)
y <- c(rep(-1, N), rep(1, N))

################################################################################

auc.conf <- AUCBoot(x, y, seed = 1)
auc.conf2 <- AUCBoot(x, y, seed = 1)

test_that("Same results of AUC with seed", {
  expect_equal(auc.conf, auc.conf2)
})

################################################################################

test_that("Same results of AUC in particular cases", {
  expect_equal(AUC(c(0, 0), 0:1), 0.5) # Equality of scores
  expect_equal(AUC(c(0.2, 0.1, 1), c(-1, -1, 1)), 1) # Perfect AUC
  expect_warning(auc1 <- AUCBoot(c(0, 0), 0:1))
  expect_equivalent(auc1, c(rep(0.5, 3), 0))
  expect_warning(auc2 <- AUCBoot(c(0.2, 0.1, 1), c(-1, -1, 1)))
  expect_equivalent(auc2, c(rep(1, 3), 0))
})

################################################################################

test_that("Same as wilcox test", {
  expect_equivalent(AUC(x, y), wilcox.test(x1, x0)$statistic / N^2)
})

################################################################################

test_that("Same as package ModelMetrics (AUC < 0.5)", {
  for (i in 1:5) {
    N <- 10^i
    x4 <- c(sample(10, size = N, replace = TRUE),
            sample(5,  size = N, replace = TRUE))
    y4 <- rep(0:1, each = N)

    expect_equivalent(AUC(x4, y4), ModelMetrics::auc(y4, x4))
  }
})

################################################################################
