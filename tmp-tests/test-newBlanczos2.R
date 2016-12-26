N <- 10000
M <- 2000
K <- 5
S <- tcrossprod(matrix(rnorm(M * K), M)) + 5 * diag(M)
X <- MASS::mvrnorm(N, mu = rep(0, M), Sigma = S)
X <- scale(X)

k <- 10
L <- k + 50
n <- nrow(X)
m <- ncol(X)
I <- 5
tol <- 1e-3

true <- svd(X, nu = k, nv = k)

# function for comparing PCs
diffPCs <- function(test, rot) {
  k <- ncol(test)
  diff1 <- 2 * abs(test - rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff2 <- 2 * abs(test + rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff <- pmin(diff1, diff2)
  mean(diff)
}

# computation of G and H
G1 <- matrix(rnorm(n * L), n, L) # G0
G2 <- matrix(rnorm(n * L), n, L) # G0
R1 <- list(crossprod(X, G1)) # m * L
R2 <- list(crossprod(X, G2))
conv <- FALSE
it <- 0
while (!conv && it < 10) {
  print(it <- it + 1) # track of time
  for (i in 1:I) {
    R1[[i + 1]] <- crossprod(X,  X %*% R1[[i]]) / n
    R2[[i + 1]] <- crossprod(X,  X %*% R2[[i]]) / n
  }
  U1.new <- svd(do.call(cbind, R1), nv = 0)$u # m * L * I
  U2.new <- svd(do.call(cbind, R2), nv = 0)$u # m * L * I

  T1.t <- X %*% U1.new # n * L * I
  T2.t <- X %*% U2.new
  T1.svd <- svd(T1.t, nu = L, nv = L)
  T2.svd <- svd(T2.t, nu = L, nv = L)
  u1 = T1.svd$u
  v1 = U1.new %*% T1.svd$v
  u2 = T2.svd$u
  v2 = U2.new %*% T2.svd$v
  diff1 <- diffPCs(u1[, 1:k], u2)
  diff2 <- diffPCs(v1[, 1:k], v2)
  print(m1 <- max(diff1, diff2))
  conv <- (m1 < tol)

  R1 <- list(v1)
  R2 <- list(v2)
} # conv of U?

test <- list(d = T1.svd$d[1:k], u = u1[, 1:k], v = v1[, 1:k])

# verif
print(all.equal(test$d, true$d[1:k]))
plot(test$u, true$u)
plot(test$v, true$v)

require(foreach)
R2 <- foreach(i = 1:k, .combine = 'cbind') %do% {
  R2.1 <- summary(lm(true$u[, i] ~ test$u[, i] - 1))$r.squared
  R2.2 <- summary(lm(true$v[, i] ~ test$v[, i] - 1))$r.squared
  c(R2.1, R2.2)
}
print(R2)
print(diffPCs(true$u, test$u))
print(diffPCs(test$v, true$v))
