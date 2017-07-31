library(bigsnpr)
X <- attach.BM(big_attachExtdata())

U <- matrix(rnorm(nrow(X) * 10), ncol = 10)

X <- as.BM.code(deepcopy(X, type = "raw"), code = c(0, 1, 2, rep(NA, 253)))

linRegPcadapt1 <- function(X, U) {
  linRegPcadapt(X, U, rows_along(X), cols_along(X))
}
true <-


linRegPcadapt2 <- function(X, U) {
  B <- crossprod(scale(U, center = TRUE, scale = FALSE), X[])
  E <- scale(X[], center = TRUE, scale = FALSE) - U %*% B
  sweep(B, 2, sqrt(colSums(E^2) / (nrow(X) - 10)), "/")
}
test <- linRegPcadapt2(X, U)

all.equal(true, t(test))


library(microbenchmark)
microbenchmark(
  linRegPcadapt1(X, U),
  linRegPcadapt2(X, U),
  times = 10
)
