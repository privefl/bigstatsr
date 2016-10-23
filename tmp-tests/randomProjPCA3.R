Rcpp::sourceCpp('src/randomProjPCA.cpp')
Rcpp::sourceCpp('src/PCA.cpp')
Rcpp::sourceCpp('src/mult.cpp')
source('R/utils.R')

require(bigsnpr)
x <- AttachBigSNP("test_doc", "../bigsnpr/backingfiles")
X <- x$genotypes

test <- RandomProjPCA(X, block.size = 1e3)

###

block.size <- 1e3
K <- 10
I <- 10
use.Eigen <- TRUE

# scaling
p <- colmeans(X) / 2
sd <- sqrt(2 * p * (1 - p))
Y <- deepcopy(X, type = "double", shared = FALSE)
scaled(Y@address, 2 * p, sd)
rm(p, sd)

# parameters
L <- 2 * K
m <- ncol(Y)
n <- nrow(Y)
I <- I + 1

# computation of G and H
H <- big.matrix(m, I * L, type = "double", shared = FALSE)
G <- matrix(rnorm(n * L), n, L) # G0
for (i in 1:I) {
  tmp.H <- BigCrossprod(Y, G, block.size,
                        use.Eigen = use.Eigen)
  H[, 1:L + (i - 1) * L] <- tmp.H
  if (i < I) G <- BigMult(Y, tmp.H, block.size,
                          use.Eigen = use.Eigen) / m
}
rm(G, tmp.H)

# svds
H.svd <- svd(H[,]) # m * L * I

check_K <- function() {
  block.size2 <- max(1, floor(n / m * block.size))
  K.H <- BigCrossprodSelf(H, block.size2)
  K.H.eigs <- eigen(K.H, symmetric = TRUE)
  for (i in 1:K) {
    diff1 <- abs(K.H.eigs$vectors[, i] - H.svd$v[, i])
    diff2 <- abs(K.H.eigs$vectors[, i] + H.svd$v[, i])
    diff <- pmin(diff1, diff2)
    if (!isTRUE(all.equal(max(diff), 0))) {
      printf("You could take K = %d\n", i - 1)
      break
    }
  }
}
check_K()
rm(H); gc()

T.t <- BigMult(Y, H.svd$u, block.size)
rm(H.svd)
T.svd <- svd(T.t, nv = 0)
sweep(T.svd$u[, 1:K], 2, (T.svd$d)[1:K], '*')















test <- RandomProjPCA(X, block.size = 1e3)

pca <- prcomp(Y)
true <- pca$x[, 1:10]

plot(as.numeric(true), as.numeric(approx), pch = 19)
plot(as.numeric(true), as.numeric(approx2[, 1:10]), pch = 19)
plot(as.numeric(true), as.numeric(approx3[, 1:10]), pch = 19)
plot(as.numeric(true), as.numeric(approx4), pch = 19)

