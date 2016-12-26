
require(foreach)
require(bigmemory)
source('R/utils.R')

X <- attach.big.matrix("tmp2.desc")
k <- 10
n <- nrow(X)
m <- ncol(X)
ncores <- 3
fun.scaling <- bigsnpr::snp_scaleBinom
cl <- parallel::makeCluster(ncores, outfile = "")
doParallel::registerDoParallel(cl)


m <- ncol(X)
intervals <- CutBySize(m, nb = ncores)

TIME <- 0.001

Ax <- big.matrix(n, 3, type = "double", shared = TRUE)
Atx <- big.matrix(m, 1, type = "double", shared = TRUE)
calc <- big.matrix(ncores, 1, type = "double", shared = TRUE, init = 0)

X.desc <- describe(X)
Ax.desc <- describe(Ax)
Atx.desc <- describe(Atx)
calc.desc <- describe(calc)

foreach(ic = 0:ncores) %dopar% {

  if (ic == 0) { # I'm the master
    Ax <- bigmemory::attach.big.matrix(Ax.desc)
    Atx.part <- bigmemory::attach.big.matrix(Atx.desc)
    calc <- bigmemory::attach.big.matrix(calc.desc)

    # A
    A <- function(x, args) {
      Atx.part[] <- x
      calc[] <- 1 # make them work
      # master wait for its slaves to finish working
      while (sum(calc[,]) > 0) Sys.sleep(TIME)
      rowSums(Ax[,])
    }
    # Atrans
    Atrans <- function(x, args) {
      Ax[, 1] <- x
      calc[] <- 2 # make them work
      # master wait for its slaves to finish working
      while (sum(calc[,]) > 0) Sys.sleep(TIME)
      Atx[,]
    }

    svds(A, k, nu = k, nv = k, opts = list(tol = tol),
         Atrans = Atrans, dim = dim(X), args = X.parts)

    calc[] <- 3 # end
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
