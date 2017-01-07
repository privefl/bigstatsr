N <- 50
M <- 100
X <- matrix(0, N, M)
X[] <- rnorm(length(X))
X2 <- scale(X)
S <- tcrossprod(X2)
# stopifnot(Matrix::rankMatrix(S) == (N - 1))

eigs <- eigen(S, symmetric = TRUE) # true values

printf <- function(...) cat(sprintf(...))
n <- 10
for (s in -10^(-5:0)) {
  eigs2 <- RSpectra::eigs_sym(S, k = n + 1, sigma = s)
  diff <- abs(eigs2$values[1:n] - tail(eigs$values[-N], n))
  printf("For sigma = %s,\n  - it took %d ops,\n", s, eigs2$nops)
  printf("  - max_diff is %e,\n  - mean_diff is %e.\n", max(diff), mean(diff))
}
