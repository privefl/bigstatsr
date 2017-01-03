################################################################################

# parallel implementation
svds4.par <- function(X, fun.scaling, ind.train, k, tol, verbose, ncores) {
  stopifnot(ncores > 1)

  n <- length(ind.train)
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

    Ax <- attach.big.matrix(Ax.desc)
    Atx <- attach.big.matrix(Atx.desc)
    calc <- attach.big.matrix(calc.desc)

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
      m.part <- up - lo + 1
      X.part <- sub.big.matrix(X.desc, firstCol = lo, lastCol = up)
      Ax <- attach.big.matrix(Ax.desc)
      Atx.part <- sub.big.matrix(Atx.desc, firstRow = lo, lastRow = up)
      calc <- attach.big.matrix(calc.desc)

      # scaling
      ms <- fun.scaling(X.part, ind.train)

      repeat {
        # slaves wait for their master to give them orders
        while (calc[ic, 1] == 0) Sys.sleep(TIME)
        c <-  calc[ic, 1]
        # slaves do the hard work
        if (c == 1) { # var?
          # compute A * x
          x <- Atx.part[,] / ms$sd
          Ax[, ic] <- pMatVec4(X.part@address, x, ind.train, 1:m.part) -
            sum(x * ms$mean)
        } else if (c == 2) {
          # compute At * x
          x <- Ax[, 1]
          Atx.part[] <- (cpMatVec4(X.part@address, x, ind.train, 1:m.part) -
                           sum(x) * ms$mean) / ms$sd
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

################################################################################

# signel cor implementation
svds4.seq <- function(X, fun.scaling, ind.train, k, tol, verbose) {
  n <- length(ind.train)
  m <- ncol(X)

  # scaling
  ms <- fun.scaling(X, ind.train)

  printf <- function(...) if (verbose) cat(sprintf(...))
  it <- 0
  # A
  A <- function(x, args) {
    printf("%d - computing A * x\n", it <<- it + 1)
    x <- x / ms$sd
    pMatVec4(X@address, x, ind.train, 1:m) - sum(x * ms$mean)
  }
  # Atrans
  Atrans <- function(x, args) {
    printf("%d - computing At * x\n", it <<- it + 1)
    (cpMatVec4(X@address, x, ind.train, 1:m) - sum(x) * ms$mean) / ms$sd
  }

  res <- RSpectra::svds(A, k, nu = k, nv = k, opts = list(tol = tol),
                        Atrans = Atrans, dim = c(n, m))

  res$means <- ms$mean
  res$sds <- ms$sd

  res
}

################################################################################

#' Randomized SVD
#'
#' An algorithm for SVD (or PCA) of a `big.matrix` based on the algorithm
#' in RSpectra (by Yixuan Qiu and Jiali Mei).
#' \cr
#' This algorithm is linear in time in all dimensions and is very
#' memory-efficient. Thus, it can be used on very large big.matrices.
#'
#'
#' @inherit bigstatsr-package params
#' @param k Number of singular vectors/values to compute. Default is `10`.
#' __This algorithm should be used to compute only a
#' few singular vectors/values.__
#' @param tol Precision parameter of [svds][RSpectra::svds].
#' Default is `1e-4`.
#' @param verbose Should some progress be printed? Default is `FALSE`.
#'
#' @export
#' @return A list of
#' - `d`, the singular values,
#' - `u`, the left singular vectors,
#' - `v`, the right singular vectors,
#' - `niter`, the number of the iteration of the algorithm,
#' - `nops`, number of Matrix-Vector multiplications used,
#' - `means`, the centering vector,
#' - `sds`, the scaling vector.
#'
#' Note that to obtain the Principal Components, you must use
#' `big_predScoresPCA` on the result. See examples.
#'
#' @example examples/example-SVD.R
#' @example examples/example-newScale.R
#' @seealso [svds][RSpectra::svds] [flashpca][flashpcaR::flashpca]
big_randomSVD <- function(X, fun.scaling,
                          ind.train = seq(nrow(X)),
                          k = 10, tol = 1e-4,
                          verbose = FALSE, ncores = 1) {
  check_X(X, ncores)

  if (ncores > 1) {
    svds4.par(X, fun.scaling, ind.train, k, tol, verbose, ncores)
  } else {
    svds4.seq(X, fun.scaling, ind.train, k, tol, verbose)
  }
}

################################################################################
