library(bigstatsr)

mtcars <- datasets::mtcars
mtcars <- mtcars[rep(1:32, 500), rep(1:11, 500)]  # 2687 Mb
# X <- FBM(nrow(mtcars), ncol(mtcars), init = 0, backingfile = "tmp-data/mtcars-big", save = TRUE)
X <- big_attach("tmp-data/mtcars-big.rds")

fill_df1 <- compiler::cmpfun(function(X, df) {
  for (j in cols_along(X)) {
    X[, j] <- df[[j]]
  }
})

tmp <- gc(reset = TRUE)
system.time(fill_df1(X, mtcars)) # 3 sec
gc() - tmp # 123 Mb


tmp <- gc(reset = TRUE)
system.time(X[] <- as.matrix(mtcars)) # 2.5 sec
gc() - tmp # 672 Mb


Rcpp::sourceCpp('tmp-tests/test-fill-from-df.cpp')

tmp <- gc(reset = TRUE)
system.time(df2FBM(X, mtcars, rows_along(X), cols_along(X))) # 1.7 sec  // 2.3
gc() - tmp  # 0 Mb

microbenchmark::microbenchmark(
  LOOP = fill_df1(X, mtcars),
  AS_MAT = X[] <- as.matrix(mtcars),
  RCPP = df2FBM(X, mtcars, rows_along(X), cols_along(X)),
  times = 10
)
## Laptop 500:
# Unit: seconds
#   expr      min       lq     mean   median       uq      max neval
#   LOOP 2.854036 2.916772 2.978472 2.964080 3.019227 3.145147    10
# AS_MAT 1.991628 2.017892 2.068286 2.055746 2.107459 2.207610    10
#   RCPP 1.230471 1.335128 1.451531 1.486146 1.495760 1.633525    10
