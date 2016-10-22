

Rcpp::sourceCpp('src/randomProjPCA.cpp')
Rcpp::sourceCpp('src/mult.cpp')
source('R/utils.R')

BigCrossprod <- function(X, mat, block.size, use.Eigen = TRUE) {
  m <- ncol(X)
  res <- matrix(0, m, ncol(mat))

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    if (use.Eigen) {
      res[ind, ] <- crossprodEigen5(X[, ind], mat)
    } else {
      res[ind, ] <- crossprod(X[, ind], mat)
    }
  }

  res
}

BigMult <- function(X, mat, block.size, use.Eigen = TRUE) {
  res <- matrix(0, nrow(X), ncol(mat))

  intervals <- CutBySize(ncol(X), block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    if (use.Eigen) { # comparer si vraiment plus rapide
      res <- incrMat(res, multEigen(X[, ind], mat[ind, ]))
    } else {
      res <- incrMat(res, X[, ind] %*% mat[ind, ])
    }
  }

  res
}

BigCrossprodSelf <- function(X, block.size2, use.Eigen = TRUE) {
  m <- ncol(X)
  res <- matrix(0, m, m)

  # function to compute X^T*X
  intervals <- CutBySize(m, block.size2)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind1 <- seq2(intervals[j, ])
    tmp1 <- X[, ind1]
    for (i in j:nb.block) {
      ind2 <- seq2(intervals[i, ])
      tmp2 <- X[, ind2]
      if (use.Eigen) {
        res[ind2, ind1] <- crossprodEigen5(tmp2, tmp1)
      } else {
        res[ind2, ind1] <- crossprod(tmp2, tmp1)
      }
    }
  }

  res
}

require(bigsnpr)
x <- AttachBigSNP("test_doc", "../bigsnpr/backingfiles")
X <- x$genotypes

###

# scaling
p <- colmeans(X) / 2
sd <- sqrt(2 * p * (1 - p))
Y <- sweep(sweep(X[,], 2, 2 * p, '-'), 2, sd, '/')

Y2 <- deepcopy(X, type = "double", shared = FALSE)
stopifnot(all.equal(X[,], Y2[,]))
stopifnot(typeof(Y2) == "double")
scaled(Y2@address, 2 * p, sd)
stopifnot(all.equal(Y, Y2[,]))

###

# parameters
K <- 10
L <- 2 * K
m <- ncol(Y)
n <- nrow(Y)
I <- 10
block.size <- 1e3
thr.eigval <- 1e-3

# algo
I <- I + 1
G.save <- matrix(rnorm(n * L), n, L) # G0

###

lH <- list()
G <- G.save
for (i in 1:I) {
  lH[[i]] <- crossprod(Y, G)
  if (i < I) G <- (Y %*% lH[[i]]) / m
}
H <- do.call(cbind, lH)

H2 <- big.matrix(m, I * L, type = "double", shared = FALSE)
G <- G.save
for (i in 1:I) {
  tmp.H <- BigCrossprod(Y2, G, block.size, use.Eigen = TRUE)
  H2[, 1:L + (i - 1) * L] <- tmp.H
  if (i < I) G <- BigMult(Y2, tmp.H, block.size, use.Eigen = TRUE) / m
}
stopifnot(all.equal(H2[,], H))

###

H.svd <- svd(H)

H2.svd <- svd(H2[,], nv = 0) # m * L * I
all.equal(H2.svd$u, H.svd$u)

# a <- matrix(0, 5e5, 220)
# a[] <- rnorm(length(a))
# print(system.time(test <- svd(a, nv = 0)))
block.size2 <- max(1, floor(n / m * block.size))
K.H2 <- BigCrossprodSelf(H2, block.size2)
K.H2.svd <- svd(K.H2)
str(K.H2.svd)
K.H2.eigs <- eigen(K.H2, symmetric = TRUE)
str(K.H2.eigs)
all.equal(K.H2.svd$v, K.H2.eigs$vectors)
plot(as.numeric(K.H2.svd$v[, 1:3]), as.numeric(K.H2.eigs$vectors[, 1:3]))

alphas <- sweep(K.H2.svd$v, 2, sqrt(K.H2.svd$d), '/')
H2.svd.u <- BigMult(H2, alphas, block.size2)
all.equal(H2.svd.u, H2.svd$u)
D <- 3; plot(as.numeric(H2.svd.u[, 1:D]), as.numeric(H2.svd$u[, 1:D]))
# still only the first two components that are equals


###

T.t <- Y %*% H.svd$u
T.svd <- svd(T.t, nv = 0)
approx <- sweep(T.svd$u[, 1:10], 2, (T.svd$d)[1:10], '*')

T2.t <- BigMult(Y2, H2.svd$u, block.size)
plot(as.numeric(T.t[, 1:5]), as.numeric(T2.t[, 1:5]), pch = 19)
K.T2.t <- crossprod(T2.t)
eigs.T2.t <- eigen(K.T2.t, symmetric = TRUE)
approx2 <- T2.t %*% eigs.T2.t$vectors
plot(as.numeric(approx[, 1:5]), as.numeric(approx2[, 1:5]), pch = 19)
# bad, bad, bad!

T3.t <- BigMult(Y2, H.svd$u, block.size)
plot(as.numeric(T.t[, 1:5]), as.numeric(T3.t[, 1:5]), pch = 19)
K.T3.t <- crossprod(T3.t)
eigs.T3.t <- eigen(K.T3.t, symmetric = TRUE)
approx3 <- T3.t %*% eigs.T3.t$vectors
plot(as.numeric(approx[, 1:5]), as.numeric(approx3[, 1:5]), pch = 19)

T4.t <- BigMult(Y2, H2.svd.u, block.size)
T4.svd <- svd(T4.t, nv = 0)
approx4 <- sweep(T4.svd$u[, 1:10], 2, (T4.svd$d)[1:10], '*')

###

pca <- prcomp(Y)
true <- pca$x[, 1:10]

plot(as.numeric(true), as.numeric(approx), pch = 19)
plot(as.numeric(true), as.numeric(approx2[, 1:10]), pch = 19)
plot(as.numeric(true), as.numeric(approx3[, 1:10]), pch = 19)
plot(as.numeric(true), as.numeric(approx4), pch = 19)

