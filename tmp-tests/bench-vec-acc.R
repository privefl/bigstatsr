library(bigstatsr)
X <- FBM(10000, 10000, init = 1)
system.time(X[1:99999999])     # 1.7 -> 1.0 -> 2.5?
system.time(X[ind <- 1:99999999 + 0]) # 1.7 -> 1.0 -> 2.5?
system.time(X[ind])

i1 <- c(1L, 1:99999999)
i2 <- i1 + 0
microbenchmark::microbenchmark(
  X[i1],
  X[i1 + 0],
  X[i2],
  times = 10
)
# Unit: milliseconds
#      expr      min       lq     mean   median        uq       max neval
#     X[i1] 756.7622 877.6568 880.8092 889.0345  907.9157  940.5456    10
# X[i1 + 0] 904.9823 937.2056 973.7816 972.8479 1004.9337 1054.5476    10
#     X[i2] 618.8485 647.3836 734.8246 724.0918  786.6781  898.4607    10
