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

nl <- c(1e3, 5e3)#, 20e3)
ml <- c(5e3)#, 20e3, 100e3)
grid <- expand.grid(n = nl, m = ml)
resSeq <- foreach(i = 1:nrow(grid), .combine = 'rbind') %do% {
  X <- fill(n = grid[i, "n"], m = grid[i, "m"])
  tmp <- microbenchmark(
    RandomProjPCA(X, fun.scaling = colmeans_sds),
    times = 10
  )
  rm(X); gc()
  tmp[, 2]
}

resPar <- foreach(i = 1:nrow(grid), .combine = 'rbind') %do% {
  X <- fill(n = grid[i, "n"], m = grid[i, "m"])
  print(system.time(
    ParallelRandomProjPCA(X, fun.scaling = colmeans_sds, ncores = 6)
  ))
  rm(X); gc()
  tmp[, 2]
}
