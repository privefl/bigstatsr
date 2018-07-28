library(bigstatsr)

N <- M <- 1000
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
  INT_NOWARN = without_downcast_warning(X[] <- mat)
)
## BEFORE:
# Unit: milliseconds
#       expr      min       lq     mean   median       uq      max neval cld
#   RAW_WARN 1.645171 1.689536 1.798087 1.761217 1.878090 2.351379   100 a
#   INT_WARN 2.720210 2.851652 3.026906 2.973326 3.096490 4.667994   100   c
# RAW_NOWARN 1.631928 1.717844 1.819296 1.763369 1.865840 2.712264   100 a
# INT_NOWARN 2.389786 2.562447 2.730060 2.681970 2.824006 3.902190   100  b
## AFTER:
# Unit: milliseconds
#       expr      min       lq     mean   median       uq      max neval cld
#   RAW_WARN 1.644839 1.751615 1.891251 1.871634 1.990991 2.407664   100 a
#   INT_WARN 2.863901 3.057091 3.476101 3.193499 3.360367 9.744881   100   c
# RAW_NOWARN 1.649144 1.747807 1.875154 1.850279 1.988674 2.317609   100 a
# INT_NOWARN 2.500368 2.702994 2.950564 2.830627 2.973822 7.222661   100  b
