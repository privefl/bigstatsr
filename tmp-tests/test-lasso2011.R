require(bigsnpr)
require(bigstatsr)

backingfile <- "../thesis-celiac/backingfiles/celiac_sub2_impute1.bk"
X2 <- AttachBigSNP(backingfile)$genotypes[1:1000, 1:1000]

require(glmnet)
X <- scale(X2)
n <- nrow(X)
m <- ncol(X)

# parameters
h2 <- 0.8 # heritability
h2.lims <- c(0.7, 0.9)
M <- 100
K <- 0.3

# simulation
v <- 1
while (v < h2.lims[1] || v > h2.lims[2]) {
  set <- sample(m, size = M)
  effects <- rnorm(M, sd = sqrt(h2 / M))
  y.simu <- scale(X[, set]) %*% effects
  print(v <- var(y.simu))
}
y.simu <- y.simu + rnorm(n, sd = sqrt(1 - v))
y.simu <- y.simu - mean(y.simu)

cp0 <- abs(crossprod(X, y.simu))
lseq <- function(from, to, N) {
  exp(seq(log(from), log(to), length.out = N))
}
lam <- lseq(max(cp0), max(cp0) / 1000, 100)

cp <- cp0
i <- 2


print(length(ind <- which(cp > (2 * lam[i] - lam[i - 1]))))
mod <- lm(y.simu ~ X[, ind] - 1)
coeffs <- mod$coefficients
coeffs[is.na(coeffs)] <- 0
pred <- X[, ind, drop = FALSE] %*% coeffs #+ mod$coefficients[1] # already
cp <- cp0 - crossprod(X, pred)
print(cp[ind])
plot(cp)
abline(h = lam[i], col = "red")
i <- i + 1
