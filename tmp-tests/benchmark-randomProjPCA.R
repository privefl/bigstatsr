#' Date: 2016-10-28
#' Object: Benchmark randomProjPCA
#' Results:

require(bigstatsr)
require(foreach)
require(microbenchmark)
source('R/utils.R')

fill <- function(n, m, block.size = 1e3) {
  X <- big.matrix(n, m, type = "double", shared = TRUE)
  intervals <- CutBySize(m, block.size)
  for (i in 1:nrow(intervals)) {
    X[, seq2(intervals[i, ])] <- rnorm(n * intervals[i, "size"])
  }

  X
}

nl <- c(1e3, 5e3, 20e3)
ml <- c(5e3, 20e3, 100e3)
grid <- expand.grid(n = nl, m = ml)
resSeq <- foreach(i = 1:nrow(grid), .combine = 'rbind') %do% {
  n <- grid[i, "n"]
  m <- grid[i, "m"]
  X <- fill(n, m)
  tmp <- microbenchmark(
    RandomProjPCA(X, fun.scaling = colmeans_sds),
    times = 10
  )
  rm(X); gc()
  c(n, m, tmp[, 2] / 1e9)
}
# 2h with R

getMeanSd <- function(x) {
  tmp <- x[-(1:2)]
  c(time.mean = mean(tmp), time.sd = sd(tmp))
}
test <- apply(resSeq, 1, getMeanSd)
res2 <- cbind(grid, t(test))

mylm <- lm(time.mean ~ n*m, data = res2)
print(summary(mylm))

mylm2 <- lm(time.mean ~ n:m - 1, data = res2)
print(summary(mylm2))

resPar <- foreach(i = 1:nrow(grid), .combine = 'rbind') %do% {
  n <- grid[i, "n"]
  m <- grid[i, "m"]
  X <- fill(n, m)
  tmp <- microbenchmark(
    ParallelRandomProjPCA(X, fun.scaling = colmeans_sds, ncores = 6),
    times = 10
  )
  rm(X); gc()
  c(n, m, tmp[, 2] / 1e9)
}
# 47 min with R

test2 <- apply(resPar, 1, getMeanSd)
res3 <- cbind(grid, t(test2))

mylm3 <- lm(time.mean ~ n*m, data = res3)
print(summary(mylm3))

mylm4 <- lm(time.mean ~ n:m, data = res3)
print(summary(mylm4))

