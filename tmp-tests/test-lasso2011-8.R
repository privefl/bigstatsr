require(bigsnpr)
require(bigstatsr)
require(biglasso)

# backingfile <- "../thesis-celiac/backingfiles/celiac_sub2_impute1.bk"
backingfile <- "../bigsnpr/backingfiles/celiac_impute1_sub1.bk"
celiac <- AttachBigSNP(backingfile)
X <- celiac$genotypes
y <- celiac$fam$affection - 1

test <- snp_scaleBinom(X)
X.svd <- big_randomSVD(X, fun.scaling = snp_scaleBinom, ncores = 4)

print(system.time(
  test1 <- biglasso(X, y, covar = NULL, # X.svd$u,
                    family = "binomial", screen = "COPY-SSR")
))


X2 <- matrix(0, 1000, 1000)
X2[] <- rnorm(length(X2))

require(glmnet)
X3 <- sweep(X2, 2, colMeans(X2), '-')
X <- sweep(X3, 2, sqrt(colSums(X3^2) / (n-1)), '/')
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
norm.y <- sqrt(sum(y.simu^2))

# rescale to unit norm (not variance)
X <- X / sqrt(n-1)
print(colSums(X^2))

# sc <- sqrt(n * (n - 1))
sc <- 1
cp0 <- abs(crossprod(X, y.simu)) / sc
# sc <- 999
lseq <- function(from, to, N) {
  exp(seq(log(from), log(to), length.out = N))
}
lam <- lseq(max(cp0), max(cp0) / 1000, 100)
print(lam[1])
print(glmnet(X, y.simu)$lambda[1])
print(lam[1]) / print(glmnet(X, y.simu)$lambda[1])

soft_thr <- function(z, g) sign(z) * max(abs(z) - g, 0)
b <- integer(length(lam))

# i <- 1
printf <- function(...) cat(sprintf(...))

time <- proc.time()
ever_ind <- integer(0)
all_betas <- numeric(ncol(X))
cp <- cp0
r <- y.simu
for (i in 2:length(lam)) {
  KKT_NOK <- TRUE
  print(i)
  print(length(ind <- which(cp > (2 * lam[i] - lam[i - 1]))))
  ever_ind <- sort(union(ever_ind, ind))
  while (KKT_NOK) {
    mat <- X[, ever_ind, drop = FALSE]
    betas <- all_betas[ever_ind]
    # j <- 1
    remains <- rep(TRUE, length(betas))
    conv <- FALSE
    tol <- 1e-4

    while (!conv) { # CD # TODO: warm starts
      conv <- TRUE
      for (j in which(remains)) {
        # print(j)
        # r <- y.simu - mat[, -j, drop = FALSE] %*% betas[-j] / sc # TIME1: 230K
        # tmpB <- crossprod(mat[, j], r) / sc + betas[j]             # TIME2: 1K
        tmpB <- crossprodCpp(mat, j, r) / sc + betas[j]
        tmp <- soft_thr(tmpB, lam[i]) # TODO: combine conds
        if (abs(diff <- (tmp - betas[j])) > tol) {
          if (abs(tmp) > tol) {
            betas[j] <- tmp
            r <- r - mat[, j] * diff # need reupdating?
          } else {
            betas[j] <- 0
            remains[j] <- FALSE
          }
          conv <- FALSE
        }
        # print(betas)
      }
    }
    # printf("After: ")
    all_betas[ever_ind] <- betas
    b[i] <- sum(betas != 0)
    # mod <- glmnet(mat, y.simu, lambda = lam[i] / sqrt(1000))
    # print(mod$beta)
    # pred <- mat %*% mod$beta
    # pred <- mat %*% betas
    r <- y.simu - mat %*% betas # reupdating (due to possible floating errors)
    cp <- abs(crossprod(X, r)) / sc
    print(length(bad_KKT <- which(cp > lam[i] * 1.02)))
    # print(set1 <- which(cp >= lam[i]))
    # print(set2 <- ever_ind[which(betas != 0)])
    # diff <- setdiff(set1, set2)
    # stopifnot(cond <- identical(set1, set2)) # TODO: add KKT fails
    # if (cond) {
    #   ind <- which(cp >= lam[i] * (1 - 1e-6))
    # }
    # print(cp[ind])
    # plot(cp)
    # abline(h = lam[i], col = "red")
    # print(ever_ind[betas != 0])

    if (length(bad_KKT)) {
      warning("BAD BAD BAD BAD BAD BAD BAD!!!!")
      ever_ind <- sort(union(ever_ind, bad_KKT))
    } else {
      KKT_NOK <- FALSE
      # other_bad_KKT <- (all_betas != 0) & (abs(cp - lam[i]) > tol * 10)
      # if (sum(other_bad_KKT)) {
      #   message("Again, BAD BAD BAD BAD BAD BAD BAD!!!!")
      #   print(lam[i] - cp[other_bad_KKT])
      # } # don't verify
    }
  }
}
print(proc.time() - time)

plot(cp)
abline(h = lam[i], col = "red")

print(b)
print(system.time(mod <- glmnet(X, y.simu, lambda = lam / sqrt(1000))))
print(colSums(mod$beta != 0))

require(biglasso)
X.big <- as.big.matrix(X)
print(system.time(mod2 <- biglasso(X.big, y.simu, lambda = lam / sqrt(1000))))
print(all.equal(mod$beta, mod2$beta[-1, ]))
