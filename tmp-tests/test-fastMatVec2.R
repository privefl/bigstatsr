require(bigsnpr)
require(bigstatsr)
require(RSpectra)
require(foreach)
source('R/utils.R')


X2 <- attach.big.matrix("tmp2.desc")
y <- rnorm(ncol(X2))

svds3 <- function(X, fun.scaling, ind.train = seq(nrow(X)),
                  k = 10, tol = 1e-4, ncores = 1) {
  m <- ncol(X)
  intervals <- CutBySize(m, nb = ncores)

  X.desc <- describe(X)
  X.parts <- foreach(ic = seq_len(ncores)) %do% {
    lims <- intervals[ic, ]
    X.part <- sub.big.matrix(X.desc,
                             firstCol = lims[1],
                             lastCol = lims[2],
                             backingpath = NULL)
    # scaling
    res <- fun.scaling(X.part, ind.train)
    res$X.part <- describe(X.part)
    res$lims <- lims

    res
  }

  cl <- parallel::makeCluster(ncores, outfile = "")
  doParallel::registerDoParallel(cl)

  foreach(ic = ind, .combine = '+') %dopar% {
    A <- 2
  }

  # A
  A <- function(x, args) {
    ind <- seq_len(length(args))
    foreach(ic = ind, .combine = '+') %dopar% {
      tmp <- args[[ic]]
      lims <- tmp$lims
      x <- x[lims[1]:lims[2]] / tmp$sd
      X <- bigmemory::attach.big.matrix(tmp$X.part)
      bigstatsr::p(X, x) - sum(x * tmp$mean)
    }
  }
  # Atrans
  Atrans <- function(x, args) {
    ind <- seq_len(length(args))
    foreach(ic = ind, .combine = 'c') %dopar% {
      tmp <- args[[ic]]
      X <- bigmemory::attach.big.matrix(tmp$X.part)
      (bigstatsr::cp(X, x) - sum(x) * tmp$mean) / tmp$sd
    }
  }

  res <- svds(A, k, nu = k, nv = k, opts = list(tol = tol),
              Atrans = Atrans, dim = dim(X), args = X.parts)

  parallel::stopCluster(cl)

  res
}

print(system.time(
  test <- svds3(X2, snp_scaleBinom, ncores = 3)
))
# 104 / 29 sec with ncores = 4
# 124 / 30 sec with ncores = 3
# 197 / 40 sec with ncores = 2

require(flashpcaR)
print(system.time(
  test2 <- flashpca("tmp2", ndim = 10, stand = "binom2", do_loadings = TRUE)
))

# true <- big_SVD(X2, snp_scaleBinom, k = 10)
print(all.equal(true$d, test$d))
plot(true$u, test$u)
s <- c(rep(FALSE, 19), TRUE)
plot(true$v[s], test$v[s])
print(diffPCs(test$u, true$u))
print(diffPCs(test$v, true$v))
