################################################################################

# Parallel implementation
svds4.par <- function(X, fun.scaling, ind.row, ind.col, k,
                      tol, verbose, ncores,
                      fun.prod, fun.cprod) {

  n <- length(ind.row)
  m <- length(ind.col)
  intervals <- CutBySize(m, nb = ncores)

  TIME <- 0.001

  Ax   <- FBM(n, ncores)
  Atx  <- FBM(m, 1)
  calc <- FBM(ncores, 1, init = 0)

  cluster_type <- getOption("bigstatsr.cluster.type")
  if (verbose) {
    register_parallel(1 + ncores, type = cluster_type, outfile = "")
  } else {
    register_parallel(1 + ncores, type = cluster_type)
  }

  res <- foreach(ic = 0:ncores) %dopar% {

    if (ic == 0) { # I'm the master

      printf <- function(...) cat(sprintf(...))
      it <- 0
      # A
      A <- function(x, args) {
        printf("%d - computing A * x\n", it <<- it + 1)
        Atx[] <- x
        calc[] <- 1  # make them work
        # Master wait for its slaves to finish working
        while (sum(calc[]) > 0) Sys.sleep(TIME)
        rowSums(Ax[])
      }
      # Atrans
      Atrans <- function(x, args) {
        printf("%d - computing At * x\n", it <<- it + 1)
        Ax[, 1] <- x
        calc[] <- 2  # make them work
        # Master wait for its slaves to finish working
        while (sum(calc[]) > 0) Sys.sleep(TIME)
        Atx[]
      }

      res <- RSpectra::svds(A, k, nu = k, nv = k, opts = list(tol = tol),
                            Atrans = Atrans, dim = c(n, m))

      calc[] <- 3 # end

      res
    } else { # You're my slaves
      # Get their part
      lo <- intervals[ic, "lower"]
      up <- intervals[ic, "upper"]
      ind.col.part <- ind.col[lo:up]

      # Scaling
      ms <- fun.scaling(X, ind.row = ind.row, ind.col = ind.col.part)

      repeat {
        # Slaves wait for their master to give them orders
        while (calc[ic] == 0) Sys.sleep(TIME)
        c <- calc[ic]
        # Slaves do the hard work
        if (c == 1) {
          # Compute A * x (part)
          Ax[, ic] <- fun.prod(X, Atx[lo:up], ind.row, ind.col.part,
                               center = ms$center, scale = ms$scale)
        } else if (c == 2) {
          # Compute At * x (part)
          Atx[lo:up] <- fun.cprod(X, Ax[, 1], ind.row, ind.col.part,
                                  center = ms$center, scale = ms$scale)
        } else if (c == 3) {
          # End
          break
        } else {
          stop("RandomSVD: unclear order from the master.")
        }
        calc[ic] <- 0
      }

      ms
    }
  }

  # Separate the results and combine the scaling vectors
  l <- do.call("c", res[-1])
  res <- res[[1]]
  s <- c(TRUE, FALSE)
  res$center <- unlist(l[s],  use.names = FALSE)
  res$scale  <- unlist(l[!s], use.names = FALSE)

  # Return
  res
}

################################################################################

# Single core implementation
svds4.seq <- function(X, fun.scaling, ind.row, ind.col, k, tol, verbose,
                      fun.prod, fun.cprod) {

  n <- length(ind.row)
  m <- length(ind.col)

  # scaling
  ms <- fun.scaling(X, ind.row, ind.col)

  printf <- function(...) if (verbose) cat(sprintf(...))
  it <- 0
  # A
  A <- function(x, args) {
    printf("%d - computing A * x\n", it <<- it + 1)
    fun.prod(X, x, ind.row, ind.col, center = ms$center, scale = ms$scale)
  }
  # Atrans
  Atrans <- function(x, args) {
    printf("%d - computing At * x\n", it <<- it + 1)
    fun.cprod(X, x, ind.row, ind.col, center = ms$center, scale = ms$scale)
  }

  res <- RSpectra::svds(A, k, nu = k, nv = k, opts = list(tol = tol),
                        Atrans = Atrans, dim = c(n, m))

  res$center <- ms$center
  res$scale  <- ms$scale

  res
}

################################################################################

#' Randomized partial SVD
#'
#' An algorithm for partial SVD (or PCA) of a Filebacked Big Matrix based on the
#' algorithm in RSpectra (by Yixuan Qiu and Jiali Mei).\cr
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
#' SIAM Journal on Matrix Analysis and Applications, 31(3), 1100-1124.
#' \url{https://doi.org/10.1137/080736417}.
#'
#' @inheritParams bigstatsr-package
#' @param k Number of singular vectors/values to compute. Default is `10`.
#' __This algorithm should be used to compute only a few singular vectors/values.__
#' @param tol Precision parameter of [svds][RSpectra::svds]. Default is `1e-4`.
#' @param verbose Should some progress be printed? Default is `FALSE`.
#' @param fun.prod Function that takes 6 arguments (in this order):
#'  - a matrix-like object `X`,
#'  - a vector `x`,
#'  - a vector of row indices `ind.row` of `X`,
#'  - a vector of column indices `ind.col` of `X`,
#'  - a vector of column centers (corresponding to `ind.col`),
#'  - a vector of column scales (corresponding to `ind.col`),
#'  and compute the product of `X` (subsetted and scaled) with `x`.
#' @param fun.cprod Same as `fun.prod`, but for the *transpose* of `X`.
#'
#' @export
#'
#' @return A named list (an S3 class "big_SVD") of
#' - `d`, the singular values,
#' - `u`, the left singular vectors,
#' - `v`, the right singular vectors,
#' - `niter`, the number of the iteration of the algorithm,
#' - `nops`, number of Matrix-Vector multiplications used,
#' - `center`, the centering vector,
#' - `scale`, the scaling vector.
#'
#' Note that to obtain the Principal Components, you must use
#' [predict][predict.big_SVD] on the result. See examples.
#'
#' @example examples/example-randomSVD.R
#' @seealso [svds][RSpectra::svds]
#'
big_randomSVD <- function(
  X, fun.scaling = big_scale(center = FALSE, scale = FALSE),
  ind.row = rows_along(X),
  ind.col = cols_along(X),
  k = 10,
  tol = 1e-4,
  verbose = FALSE,
  ncores = 1,
  fun.prod = big_prodVec,
  fun.cprod = big_cprodVec
) {

  check_args(X = "")

  if (ncores > 1) {
    res <- svds4.par(X, fun.scaling, ind.row, ind.col, k, tol, verbose, ncores,
                     fun.prod, fun.cprod)
  } else {
    res <- svds4.seq(X, fun.scaling, ind.row, ind.col, k, tol, verbose,
                     fun.prod, fun.cprod)
  }

  structure(res, class = "big_SVD")
}

################################################################################
