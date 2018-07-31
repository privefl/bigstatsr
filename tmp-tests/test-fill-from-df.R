library(bigstatsr)

mtcars <- datasets::mtcars
mtcars <- mtcars[rep(1:32, 500), rep(1:11, 500)]
# X <- FBM(nrow(mtcars), ncol(mtcars), init = 0,
#          backingfile = "tmp-data/mtcars-big2", save = TRUE)
X <- big_attach("tmp-data/mtcars-big.rds")
stopifnot(identical(dim(X), dim(mtcars)))

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


Rcpp::sourceCpp('src/replace.cpp')

# X[] <- 0
tmp <- gc(reset = TRUE)
system.time(replaceDF(X$address, rows_along(X), cols_along(X), mtcars)) # 1.7 sec  // 2.3
gc() - tmp  # 0 Mb


Rcpp::sourceCpp('tmp-tests/test-col-acc.cpp')
tmp <- gc(reset = TRUE)
system.time(col_acc(X, mtcars)) # 1.7 sec  // 2.3
gc() - tmp  # 0 Mb


microbenchmark::microbenchmark(
  LOOP = fill_df1(X, mtcars),
  AS_MAT = X[] <- as.matrix(mtcars),
  RCPP_MAT = X[] <- mtcars,
  # RCPP_MAT = df2FBM(X, mtcars, rows_along(X), cols_along(X)),
  # RCPP_COL = col_acc(X, mtcars),
  times = 10
)
## Laptop 500:
# Unit: seconds
#   expr      min       lq     mean   median       uq      max neval
#   LOOP 2.854036 2.916772 2.978472 2.964080 3.019227 3.145147    10
# AS_MAT 1.991628 2.017892 2.068286 2.055746 2.107459 2.207610    10
#   RCPP 1.230471 1.335128 1.451531 1.486146 1.495760 1.633525    10
## Windows 500:
# Unit: milliseconds
#   expr       min        lq      mean    median        uq       max neval cld
#   LOOP 1111.7148 1126.4525 1164.2682 1159.3087 1185.7340 1230.4831    10   c
# AS_MAT  381.4052  392.1457  453.9552  469.8487  513.6153  527.1346    10  b
#   RCPP  127.1239  128.5628  129.8337  129.0010  129.8564  137.2489    10 a
## Windows 1000:
# Unit: milliseconds
#   expr       min        lq      mean    median        uq       max neval cld
#   LOOP 2219.3142 2261.2156 2379.2766 2352.9082 2517.6354 2609.5117    10   c
# AS_MAT  757.9630  773.1036  912.3734  920.8044 1049.7320 1066.9763    10  b
#   RCPP  250.6258  252.6100  265.4693  263.4575  278.1126  282.5939    10 a
