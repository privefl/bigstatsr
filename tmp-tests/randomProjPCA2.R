N <- 100
M <- 1000
K.PC <- 11
s <- matrix(rnorm(K.PC * M), K.PC, M)
sigma <- crossprod(s)
X <- MASS::mvrnorm(N, mu = rep(0, M), Sigma = sigma)
rm(s, sigma)

X <- matrix(rnorm(N * M), N, M) # no structure

require(bigsnpr)
Y2 <- as.big.matrix(X, type = "double", shared = FALSE)
meansds <- colmeans_sds(Y2)
scaled(Y2@address, meansds$mean, meansds$sd)
print(range(apply(Y2[,], 2, mean)))
print(range(apply(Y2[,], 2, sd)))

# parameters
K <- 10
L <- 2 * K
m <- ncol(Y2)
n <- nrow(Y2)
I <- 10
block.size <- 1e3
thr.eigval <- 1e-3

# algo
I <- I + 1
G.save <- matrix(rnorm(n * L), n, L) # G0

###

H2 <- big.matrix(m, I * L, type = "double", shared = FALSE)
G <- G.save
for (i in 1:I) {
  tmp.H <- BigCrossprod(Y2, G, block.size, use.Eigen = TRUE)
  H2[, 1:L + (i - 1) * L] <- tmp.H
  if (i < I) G <- BigMult(Y2, tmp.H, block.size, use.Eigen = TRUE) / m
}

###

H2.svd <- svd(H2[,], nv = 0) # m * L * I

block.size2 <- max(1, floor(n / m * block.size))
K.H2 <- BigCrossprodSelf(H2, block.size2)
# K.H2.svd <- svd(K.H2)
# str(K.H2.svd)
K.H2.eigs <- eigen(K.H2, symmetric = TRUE)
str(K.H2.eigs)
plot(log(K.H2.eigs$values))
all.equal(K.H2.svd$v, K.H2.eigs$vectors)
plot(as.numeric(K.H2.svd$v[, 1:3]), as.numeric(K.H2.eigs$vectors[, 1:3]))

# alphas <- sweep(K.H2.svd$v, 2, sqrt(K.H2.svd$d), '/')
# H2.svd.u <- BigMult(H2, alphas, block.size2)
alphas <- sweep(K.H2.eigs$vectors[, 1:20], 2, sqrt(K.H2.eigs$values[1:20]), '/')
H2.svd.u <- BigMult(H2, alphas, block.size2)
all.equal(H2.svd.u, H2.svd$u[, 1:20])
D <- 20; plot(as.numeric(H2.svd.u[, 1:D]), as.numeric(H2.svd$u[, 1:D]))
######################
# equals until K.PCs # No structure -> min(N, M)
######################
# How many PCs are necessary to capture all the variations in the data?
