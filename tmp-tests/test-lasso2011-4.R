Rcpp::sourceCpp('tmp-tests/lasso.cpp')

X2 <- matrix(0, 1000, 1000)
X2[] <- rnorm(length(X2))
n <- nrow(X2)
m <- ncol(X2)

require(glmnet)
X3 <- sweep(X2, 2, colMeans(X2), '-')
X <- sweep(X3, 2, sqrt(colSums(X3^2) / (n-1)), '/')

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
  y.simu <- X[, set] %*% effects
  print(v <- var(y.simu))
}
y.simu <- y.simu + rnorm(n, sd = sqrt(1 - v))
y.simu <- y.simu - mean(y.simu)
norm.y <- sqrt(sum(y.simu^2))

# rescale to unit norm (not variance)
X <- X / sqrt(n-1)
print(colSums(X^2))

# sc <- sqrt(n * (n - 1))
sc <- 1
tol <- 1e-4
cp0 <- abs(crossprod(X, y.simu)) / sc
# sc <- 999
lseq <- function(from, to, N) {
  exp(seq(log(from), log(to), length.out = N))
}
lam <- lseq(max(cp0), max(cp0) / 1000, 100)
print(lam[1])
print(glmnet(X, y.simu)$lambda[1])
print(lam[1]) / print(glmnet(X, y.simu)$lambda[1])

b <- integer(length(lam))

printf <- function(...) cat(sprintf(...))


time <- proc.time()
A_ind <- which(cp0 > (2 * lam[2] - lam[1]))
all_betas <- numeric(ncol(X))
cp <- cp0
r <- y.simu
for (i in 2:length(lam)) {
  printf("i = %d\n", i)
  eps_ind <- A_ind
  seq_strong_thr <- 2 * lam[i] - lam[i - 1]
  KKT_thr <- lam[i] * 1.02

  while (TRUE) {
    printf("Length of eps_ind: %d\n", length(eps_ind))
    mat <- X[, eps_ind, drop = FALSE]
    betas.old <- all_betas[eps_ind]
    # printf("Before: "); print(betas)
    betas.new <- CD_lasso_Cpp(mat, r, all_betas[eps_ind], lam[i], sc, tol)
    all_betas[eps_ind] <- betas.new
    # printf("After: "); print(betas)
    # if (i == 10) stop("Greve!")
    ind0 <- (betas.old == 0)
    printf("Ratio: %.2g\n", mean(betas.new[!ind0] / betas.old[!ind0]))

    r <- y.simu - mat %*% betas.new # reupdating (due to possible floating errors)
    cp <- abs(crossprod(X, r)) / sc

    # step c
    print(length(bad_KKT_c <- which(cp > max(seq_strong_thr, KKT_thr))))
    if (length(bad_KKT_c)) {
      # Add these predictors
      # sorting by appearance? and avoing recopying the matrix
      eps_ind <- sort(union(eps_ind, bad_KKT_c))
      next
    }

    # step d
    print(length(bad_KKT_d <- which(cp > KKT_thr)))
    if (length(bad_KKT_d)) {
      # Add these predictors
      # sorting by appearance? and avoing recopying the matrix
      eps_ind <- sort(union(eps_ind, bad_KKT_d))
      next
    }

    A_ind <- sort(union(A_ind, eps_ind[betas != 0])) # really that?
    b[i] <- sum(betas != 0)
    break
  }
}
print(proc.time() - time)


plot(cp)
abline(h = lam[i], col = "red")


print(system.time(mod <- glmnet(X, y.simu, lambda = lam / sqrt(1000), dfmax = 300)))
print(colSums(mod$beta != 0))

require(biglasso)
X.big <- as.big.matrix(X)
print(system.time(mod2 <- biglasso(X.big, y.simu, lambda = lam / sqrt(1000), dfmax = 100)))
print(all.equal(mod$beta, mod2$beta[-1, ]))

print(cbind(b, colSums(mod$beta != 0), colSums(mod2$beta != 0)))
