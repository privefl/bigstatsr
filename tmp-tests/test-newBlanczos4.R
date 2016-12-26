N <- 20000
M <- 2000
K <- 5
set.seed(NULL)
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
set.seed(1)
G <- list(matrix(rnorm(n * L), n, L)) # G0: n * L
conv <- FALSE
it <- 0
R <- list()
while (!conv && it < 10) {
  print(it <- it + 1) # track of time
  for (i in 1:I) {
    R[[i]] <- crossprod(X, G[[i]])
    G[[i + 1]] <- X %*% R[[i]] / m
  }
  U1 <- svd(do.call(cbind, G), nv = 0)$u # n * L * (I + 1)
  U2 <- svd(do.call(cbind, R), nv = 0)$u # m * L * I

  T1.t <- crossprod(X, U1) # m * L * (I + 1)
  T2.t <- X %*% U2 # n * L * I
  T1.svd <- svd(T1.t, nu = L, nv = L) # m
  T2.svd <- svd(T2.t, nu = L, nv = L) # n
  u1 = U1 %*% T1.svd$v
  v1 = T1.svd$u
  u2 = T2.svd$u
  v2 = U2 %*% T2.svd$v
  diff1 <- diffPCs(u1[, 1:k], u2)
  diff2 <- diffPCs(v1[, 1:k], v2)
  print(m1 <- max(diff1, diff2))
  conv <- (m1 < tol)

  G <- list(u1)
} # Best way?


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


