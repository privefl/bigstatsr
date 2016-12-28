require(bigsnpr)
require(bigstatsr)
require(RSpectra)
require(foreach)
source('R/utils.R')


X2 <- attach.big.matrix("tmp.desc")
# y1 <- rnorm(nrow(X2))
# y2 <- rnorm(ncol(X2))
# require(microbenchmark)
# print(microbenchmark(
#   test1 <- produ(X2@address, y2),
#   test2 <- produ2(X2@address, y2),
#   test3 <- crossprodu(X2@address, y1),
#   test4 <- crossprodu2(X2@address, y1),
#   times = 3
# )) # same time mano vs Eigen

svds4 <- function(X, fun.scaling, k = 10, tol = 1e-4,
                  ncores = parallel::detectCores() - 1,
                  verbose = TRUE) {
  stopifnot(ncores > 1)
  # stopifnot(ncores < parallel::detectCores())

  n <- nrow(X)
  m <- ncol(X)
  intervals <- CutBySize(m, nb = ncores)

  TIME <- 0.001

  Ax <- big.matrix(n, ncores, type = "double", shared = TRUE)
  Atx <- big.matrix(m, 1, type = "double", shared = TRUE)
  calc <- big.matrix(ncores, 1, type = "double", shared = TRUE, init = 0)

  X.desc <- describe(X)
  Ax.desc <- describe(Ax)
  Atx.desc <- describe(Atx)
  calc.desc <- describe(calc)

  if (verbose) {
    cl <- parallel::makeCluster(1 + ncores, outfile = "")
  } else {
    cl <- parallel::makeCluster(1 + ncores)
  }
  doParallel::registerDoParallel(cl)

  res <- foreach(ic = 0:ncores) %dopar% {

    Ax <- bigmemory::attach.big.matrix(Ax.desc)
    Atx <- bigmemory::attach.big.matrix(Atx.desc)
    calc <- bigmemory::attach.big.matrix(calc.desc)

    if (ic == 0) { # I'm the master
      printf <- function(...) cat(sprintf(...))
      it <- 0
      # A
      A <- function(x, args) {
        printf("%d - computing A * x\n", it <<- it + 1)
        Atx[] <- x
        calc[] <- 1 # make them work
        # master wait for its slaves to finish working
        while (sum(calc[,]) > 0) Sys.sleep(TIME)
        rowSums(Ax[,])
      }
      # Atrans
      Atrans <- function(x, args) {
        printf("%d - computing At * x\n", it <<- it + 1)
        Ax[, 1] <- x
        calc[] <- 2 # make them work
        # master wait for its slaves to finish working
        while (sum(calc[,]) > 0) Sys.sleep(TIME)
        Atx[,]
      }

      res <- RSpectra::svds(A, k, nu = k, nv = k, opts = list(tol = tol),
                            Atrans = Atrans, dim = c(n, m))

      calc[] <- 3 # end

      res
    } else { # You're my slaves
      # get their part
      lo <- intervals[ic, "lower"]
      up <- intervals[ic, "upper"]
      X.part <- bigmemory::sub.big.matrix(X.desc, firstCol = lo,
                                          lastCol = up)
      Ax <- bigmemory::attach.big.matrix(Ax.desc)
      Atx.part <- bigmemory::sub.big.matrix(Atx.desc, firstRow = lo,
                                            lastRow = up)
      calc <- bigmemory::attach.big.matrix(calc.desc)

      # scaling
      ms <- fun.scaling(X.part)

      repeat {
        # slaves wait for their master to give them orders
        while (calc[ic, 1] == 0) Sys.sleep(TIME)
        c <-  calc[ic, 1]
        # slaves do the hard work
        if (c == 1) { # var?
          # compute A * x
          x <- Atx.part[,] / ms$sd
          Ax[, ic] <- bigstatsr::p(X.part, x) - sum(x * ms$mean)
        } else if (c == 2) {
          # compute At * x
          x <- Ax[, 1]
          Atx.part[] <- (bigstatsr::cp(X.part, x) - sum(x) * ms$mean) / ms$sd
        } else { # end
          break
        }
        calc[ic, 1] <- 0
      }

      ms
    }
  }

  parallel::stopCluster(cl)

  l <- do.call(c, res[-1])
  res <- res[[1]]
  s <- c(TRUE, FALSE)
  res$means <- unlist(l[s], use.names = FALSE)
  res$sds <- unlist(l[!s], use.names = FALSE)

  res
}

print(system.time(
  test <- svds4(X2, snp_scaleBinom, ncores = 4, k = 10)
))
# 85 sec with ncores = 4 -> 190 sec with k = 20
# 109 sec with ncores = 3
# 148 sec with ncores = 2

# require(flashpcaR)
# print(system.time(
#   test2 <- flashpca("tmp", ndim = 20, stand = "binom2", do_loadings = TRUE)
# )) # 139 sec -> 244 with ndim = 20
#
# true <- big_SVD(X2, snp_scaleBinom, k = 10)
# print(all.equal(true$d, test$d))
# plot(true$u, test$u)
# s <- c(rep(FALSE, 19), TRUE)
# plot(true$v[s], test$v[s])
# print(diffPCs(test$u, true$u))
# print(diffPCs(test$v, true$v))

# celiac <- AttachBigSNP("../bigsnpr/backingfiles/celiac_impute1_sub1.bk")
# X3 <- celiac$genotypes
# print(system.time(
#   test3 <- svds4(X3, snp_scaleBinom, ncores = 3, k = 10, verbose = TRUE)
# ))
