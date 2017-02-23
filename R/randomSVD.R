################################################################################

# parallel implementation
svds4.par <- function(X.desc, fun.scaling, ind.row, ind.col,
                      k, tol, verbose, ncores) {
  n <- length(ind.row)
  m <- length(ind.col)
  intervals <- CutBySize(m, nb = ncores)

  TIME <- 0.001

  Ax.desc <- tmpFBM(n, ncores)
  Atx.desc <- tmpFBM(m, 1)
  calc.desc <- tmpFBM(ncores, 1, init = 0)

  if (verbose) {
    cl <- parallel::makeCluster(1 + ncores, outfile = "")
  } else {
    cl <- parallel::makeCluster(1 + ncores)
  }
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  res <- foreach(ic = 0:ncores) %dopar% {
    if (ic == 0) { # I'm the master
      Ax <- attach.big.matrix(Ax.desc)
      Atx <- attach.big.matrix(Atx.desc)
      calc <- attach.big.matrix(calc.desc)

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
      ind.col.part <- ind.col[lo:up]
      X <- attach.big.matrix(X.desc)
      Ax <- attach.big.matrix(Ax.desc)
      Atx.part <- sub.big.matrix(Atx.desc, firstRow = lo, lastRow = up)
      calc <- attach.big.matrix(calc.desc)

      # scaling
      ms <- fun.scaling(X, ind.row = ind.row, ind.col = ind.col.part)

      repeat {
        # slaves wait for their master to give them orders
        while (calc[ic, 1] == 0) Sys.sleep(TIME)
        c <-  calc[ic, 1]
        # slaves do the hard work
        if (c == 1) {
          # compute A * x
          x <- Atx.part[,] / ms$sd
          Ax[, ic] <- pMatVec4(X@address, x, ind.row, ind.col.part) -
            sum(x * ms$mean)
        } else if (c == 2) {
          # compute At * x
          x <- Ax[, 1]
          Atx.part[] <- (cpMatVec4(X@address, x, ind.row, ind.col.part) -
                           sum(x) * ms$mean) / ms$sd
        } else if (c == 3) { # end
          break
        } else {
          stop("RandomSVD: unclear order from the master.")
        }
        calc[ic, 1] <- 0
      }

      ms
    }
  }

  # separate the results and combine the scaling vectors
  l <- do.call('c', res[-1])
  res <- res[[1]]
  s <- c(TRUE, FALSE)
  res$means <- unlist(l[s], use.names = FALSE)
  res$sds <- unlist(l[!s], use.names = FALSE)

  # remove temporary files
  unlink2 <- function(desc) {
    desc <- desc@description
    file.root <- file.path(desc$dirname, desc$filename)
    unlink(paste0(file.root, c("", ".desc")))
  }
  sapply(c(Ax.desc, Atx.desc, calc.desc), unlink2)

  # return
  res
}

################################################################################

# single core implementation
svds4.seq <- function(X., fun.scaling, ind.row, ind.col, k, tol, verbose) {
  n <- length(ind.row)
  m <- length(ind.col)
  X <- attach.BM(X.)

  # scaling
  ms <- fun.scaling(X, ind.row, ind.col)

  printf <- function(...) if (verbose) cat(sprintf(...))
  it <- 0
  # A
  A <- function(x, args) {
    printf("%d - computing A * x\n", it <<- it + 1)
    x <- x / ms$sd
    pMatVec4(X@address, x, ind.row, ind.col) - sum(x * ms$mean)
  }
  # Atrans
  Atrans <- function(x, args) {
    printf("%d - computing At * x\n", it <<- it + 1)
    (cpMatVec4(X@address, x, ind.row, ind.col) - sum(x) * ms$mean) / ms$sd
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
#' @note The idea of using this Implicitly Restarted Arnoldi Method algorithm
#' comes from G. Abraham, Y. Qiu, and M. Inouye,
#' FlashPCA2: principal component analysis of biobank-scale genotype datasets,
#' bioRxiv: \url{https://doi.org/10.1101/094714}.
#' \cr
#' It proved to be faster than our implementation of the "blanczos" algorithm
#' in Rokhlin, V., Szlam, A., & Tygert, M. (2010).
#' A Randomized Algorithm for Principal Component Analysis.
#' SIAM Journal on Matrix Analysis and Applications, 31(3), 1100–1124.
#' \url{http://dx.doi.org/10.1137/080736417}.
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
#' @example examples/example-randomSVD.R
#' @seealso [svds][RSpectra::svds]
big_randomSVD <- function(X., fun.scaling,
                          ind.row = rows_along(X.),
                          ind.col = cols_along(X.),
                          k = 10, tol = 1e-4,
                          verbose = FALSE, ncores = 1) {
  if (ncores > 1) {
    svds4.par(describe(X.), fun.scaling, ind.row, ind.col,
              k, tol, verbose, ncores)
  } else {
    svds4.seq(X., fun.scaling, ind.row, ind.col,
              k, tol, verbose)
  }
}

################################################################################
