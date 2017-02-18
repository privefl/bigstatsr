################################################################################

context("AUC")

N <- 100
x0 <- rnorm(N, mean = runif(1))
x1 <- rnorm(N, mean = 2*runif(1))
x <- c(x0, x1)
y <- c(rep(-1, N), rep(1, N))

################################################################################

auc <- big_aucSample(x, y, nsim = 1e6, seed = 1)
auc2 <- big_aucSample(x, y, nsim = 1e6, seed = 1)
auc.conf <- big_aucSampleConf(x, y, nboot = 1e4, nsim = 1e3, seed = 1)
auc.conf2 <- big_aucSampleConf(x, y, nboot = 1e4, nsim = 1e3, seed = 1)

test_that("Same results of AUC with seed", {
  expect_equal(auc, auc2)
  expect_equal(auc.conf, auc.conf2)
})

################################################################################

repl <- replicate(1e4, {
  ind <- sample(2*N, replace = TRUE)
  big_aucSample(x[ind], y[ind], nsim = 1e3)
})

repl.conf <- c("Mean" = mean(repl),
               quantile(repl, probs = c(0.025, 0.975)),
               "Sd" = sd(repl))

test_that("Same results than simple bootstrap implementation", {
  expect_equal(repl.conf, auc.conf, tolerance = 1e-2) # OK over 1000 runs
})

################################################################################

test_that("Same results of AUC in particular cases", {
  expect_equal(big_aucSample(c(0, 0), 0:1), 0.5) # Equality of scores
  expect_equal(big_aucSample(c(0.2, 0.1, 1), c(-1, -1, 1)), 1) # Perfect AUC
  expect_equivalent(big_aucSampleConf(c(0, 0), 0:1, nboot = 1e3),
                    c(rep(0.5, 3), 0))
  expect_equivalent(
    big_aucSampleConf(c(0.2, 0.1, 1), c(-1, -1, 1), nboot = 1e3),
                    c(rep(1, 3), 0))
})

################################################################################
