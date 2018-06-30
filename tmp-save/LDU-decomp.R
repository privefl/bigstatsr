N <- 1000
M <- 10
x <- runif(N)
X <- matrix(runif(N * M), N, M)

U <- svd(X)$u
crossprod(U)

X2 <- cbind(x, U)
(K <- crossprod(X2))

(K2 <- solve(K))
# solve(K, t(X2))

# https://en.wikipedia.org/wiki/Woodbury_matrix_identity#Derivation_from_LDU_decomposition
V <- drop(crossprod(x, U))
alpha <- 1 / drop(crossprod(x) - crossprod(V))
-alpha * V
K2[-1, 1]

alpha * tcrossprod(V) + diag(M)
K2[-1, -1]

y <- sample(0:1, N, TRUE)
# precompute some estimation with only the covariables (and the intercept)
mod0 <- stats::glm(y ~ X2[, -1] - 1, family = "binomial")
p0 <- mod0$fitted
w0 <- p0 * (1 - p0)
z0 <- log(p0 / (1 - p0)) * w0 + (y - p0)
beta0 <- solve(crossprod(X2, sweep(X2, 1, w0, '*')),
               crossprod(X2, y - p0))
plot(beta0, c(0, mod0$coefficients))
## IRLS
beta <- rep(0, M + 1)
c <- 1
repeat{
  p <- 1 / (1 + exp(-(X2 %*% beta)))
  w <- p * (1 - p)
  cprod <- crossprod(X2, sweep(X2, 1, w, '*'))
  inv <- solve(cprod)
  diff <- inv %*% crossprod(X2, y - p)
  diff.max <- max(abs(diff / beta))
  print(res <- c(c, inv[1, 1], diff.max))
  if (diff.max < 1e-8) {
    break
  } else {
    c <- c + 1
  }
  beta <- beta + diff
}

## MM
beta2 <- rep(0, M + 1)
# beta2 <- c(0, mod0$coefficients)
c2 <- 1
repeat{
  p2 <- 1 / (1 + exp(-(X2 %*% beta2)))
  inv2 <- 4 * solve(crossprod(X2))
  diff2 <- inv2 %*% crossprod(X2, y - p2)
  diff2.max <- max(abs(diff2 / beta2))
  print(res2 <- c(c2, inv2[1, 1], diff2.max))
  if (diff2.max < 1e-8) {
    break
  } else {
    c2 <- c2 + 1
  }
  beta2 <- beta2 + diff2
}
print(res / res2)
p3 <- 1 / (1 + exp(-(X2 %*% beta2)))
w3 <- p3 * (1 - p3)
cprod3 <- crossprod(X2, sweep(X2, 1, w3, '*'))
inv3 <- solve(cprod3)
diff3 <- inv3 %*% crossprod(X2, y - p3)
diff3.max <- max(abs(diff3 / beta2))
print(c(c, c2))
print(c(beta[1], beta2[1]))
print(c(diff.max, diff2.max, diff3.max))
print(c(inv[1, 1], inv2[1, 1], inv3[1, 1]))

