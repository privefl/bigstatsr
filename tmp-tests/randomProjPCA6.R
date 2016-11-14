#' Date: 2016-11-08
#' Object: Test randomSVD when n > m
#' Results:

M <- 751
N <- 2003
x <- matrix(rnorm(N*M, sd = 5), N)
X <- as.big.matrix(x, type = "char")
test <- big_randomSVD(X = X,
                      block.size = 100,
                      fun.scaling = big_noscale,
                      K = 20)

K <- 2
ind.train <- seq(nrow(X))
I <- 10
fun.scaling <- big_noscale
block.size <- 100

# parameters
L <- K + 12
n <- length(ind.train)
m <- ncol(X)
I <- I + 1

# scaling
stats <- fun.scaling(X, ind.train)
means <- stats$mean
sds <- stats$sd
rm(stats)

# computation of G and H
H <- list()
tmp <- list(G = matrix(rnorm(n * L), n, L)) # G0
for (i in 1:I) {
  tmp <- getHnG(X, tmp$G, ind.train, block.size, means, sds,
                use.Eigen = TRUE)
  H[i] <- tmp['H']
}
rm(tmp)

# svds
H.svd <- svd(do.call(cbind, H), nv = 0) # m * L * I
rm(H); gc()

T.t <- BigMult2(X, H.svd$u, ind.train, block.size, means, sds,
                use.Eigen = use.Eigen)
T.svd <- svd(T.t, nu = K, nv = K)

list(d = T.svd$d[1:K], u = T.svd$u, v = H.svd$u %*% T.svd$v,
     means = means, sds = sds)
