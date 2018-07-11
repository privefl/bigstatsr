library(bigstatsr)

X <- FBM(1e4, 1e4, type = "integer", init = 1)
X3 <- FBM(1e4, 1e4, type = "double", init = 1)

microbenchmark::microbenchmark(
  INT = X2 <- big_copy(X),
  REAL = X4 <- big_copy(X3, type = "integer"),
  times = 10
)
# Unit: seconds
#  expr      min       lq     mean   median       uq      max neval
#   INT 2.024746 2.402898 3.037447 2.837671 3.540500 4.539352    10
#  REAL 2.750987 3.031049 3.411538 3.259727 3.671679 4.669749    10

system.time(
  X[] <- X[]
)  # 1 sec
system.time(
  X3[] <- X[]
)  # 3-5 sec
system.time(
  X[] <- X3[]
)  # 2 sec
system.time(
  X3[] <- X3[]
)  # 2-3 sec

x <- X[]
x3 <- X3[]
system.time(
  X[] <- x
)  # 0.5-0.7 sec
system.time(
  X3[] <- x
)  # 2-3.4 sec
system.time(
  X[] <- x3
)  # 1.4 sec
system.time(
  X3[] <- x3
)  # 1.6 sec
