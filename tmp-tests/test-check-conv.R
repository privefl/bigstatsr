library(bigstatsr)
options(bigstatsr.downcast.warning = TRUE)

N <- M <- 2000
mat <- matrix(runif(N * M, 0, 256), N, M)
storage.mode(mat) <- "integer"

mat2 <- mat
storage.mode(mat2) <- "raw"

X <- big_copy(mat2)
typeof(X)
typeof(mat)

microbenchmark::microbenchmark(
  RAW_WARN = X[] <- mat2,
  INT_WARN = X[] <- mat,
  RAW_NOWARN = without_downcast_warning(X[] <- mat2),
  INT_NOWARN = without_downcast_warning(X[] <- mat),
  times = 200
)
## BEFORE:
# Unit: milliseconds
#       expr       min        lq      mean    median        uq      max neval
#   RAW_WARN  8.850176   9.56644  9.705273  9.651982  9.732185 14.95322   200
#   INT_WARN 16.952247  18.15335 18.999498 18.551330 18.729151 33.46362   200
# RAW_NOWARN  9.053073   9.60809  9.791535  9.680127  9.812488 14.17877   200
# INT_NOWARN 14.892180  16.54560 17.046054 16.682986 16.825069 29.61774   200
## AFTER:
# Unit: milliseconds
#       expr       min        lq      mean    median        uq      max neval
#   RAW_WARN  8.249141  8.539110 10.091688  8.828015 10.613616 15.80058   200
#   INT_WARN 11.808361 11.913297 13.804105 12.465170 13.976114 22.31180   200
# RAW_NOWARN  8.245795  8.548852  9.809506  8.794139  9.856534 15.98896   200
# INT_NOWARN  8.514903  8.602085  9.776280  8.880168  9.370276 15.32295   200
