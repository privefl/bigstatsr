library(bigstatsr)

# mtcars <- datasets::mtcars
# mtcars <- mtcars[rep(1:32, 500), rep(1:11, 500)]  # 2687 Mb
# X <- FBM(nrow(mtcars), ncol(mtcars), backingfile = "tmp-data/mtcars-big", save = TRUE)
X <- big_attach("tmp-data/mtcars-big.rds")

tmp <- gc(reset = TRUE)
system.time({
  for (j in cols_along(mtcars)) {
    X[, j] <- mtcars[[j]]
  }
}) # 5 sec  -> too long  //  3.8
gc() - tmp # 337 Mb  // 144 Mb


tmp <- gc(reset = TRUE)
system.time({
  X[] <- as.matrix(mtcars)
}) # 2.8 sec  //  5.8
gc() - tmp # 2687 Mb  -> too much memory (full copy)  // 672


Rcpp::sourceCpp('tmp-tests/test-fill-from-df.cpp')

tmp <- gc(reset = TRUE)
system.time({
  df2FBM(X, mtcars, rows_along(X3), cols_along(X3))
}) # 1.7 sec  // 4.5
gc() - tmp  # 0 Mb  -> perfect

microbenchmark::microbenchmark(
  LOOP = {
    for (j in cols_along(mtcars)) {
      X[, j] <- mtcars[[j]]
    }
  },
  AS_MAT = X[] <- as.matrix(mtcars),
  RCPP = df2FBM(X, mtcars, rows_along(X3), cols_along(X3)),
  times = 10
)
## Laptop:
# Unit: seconds
#   expr      min       lq     mean   median       uq      max neval
#   LOOP 2.775548 2.862055 2.900700 2.889623 2.930390 3.047641    10
# AS_MAT 1.962036 1.993375 2.038083 2.024001 2.096362 2.117451    10
#   RCPP 1.193567 1.321788 1.616395 1.501757 1.611372 2.389710    10
