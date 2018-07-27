library(bigstatsr)

mtcars <- datasets::mtcars
mtcars <- mtcars[rep(1:32, 1000), rep(1:11, 1000)]  # 2687 Mb


tmp <- gc(reset = TRUE)
X <- FBM(nrow(mtcars), ncol(mtcars))
system.time({
  for (j in cols_along(mtcars)) {
    X[, j] <- mtcars[[j]]
  }
}) # 5 sec  -> too long
gc() - tmp # 337 Mb


tmp <- gc(reset = TRUE)
X2 <- FBM(nrow(mtcars), ncol(mtcars))
system.time({
  X2[] <- as.matrix(mtcars)
}) # 2.8 sec
gc() - tmp # 2687 Mb  -> too much memory (full copy)


Rcpp::sourceCpp('tmp-tests/test-fill-from-df.cpp')

tmp <- gc(reset = TRUE)
X3 <- FBM(nrow(mtcars), ncol(mtcars))
system.time({
  df2FBM(X3, mtcars, rows_along(X3), cols_along(X3))
}) # 1.7 sec
gc() - tmp  # 0 Mb  -> perfect
