library(bigstatsr)

N <- 100; M <- 10
X <- FBM(N, M, init = rnorm(N * M))
X[, 1] <- 1

y <- rnorm(N)
y01 <- sample(0:1, N, TRUE)

(gwas <- big_univLinReg(X, y))
predict(gwas, log10 = FALSE)

(gwas2 <- big_univLogReg(X, y01))
predict(gwas2, log10 = FALSE)
