################################################################################

getHnG <- function(X, G.old, ind.train, block.size,
                   vec.center, vec.scale, use.Eigen) {
  n <- length(ind.train)
  m <- ncol(X)
  m2 <- ncol(G.old)
  H <- matrix(0, m, m2)
  G <- matrix(0, n, m2)

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.train, ind], vec.center[ind], vec.scale[ind])
    if (use.Eigen) {
      G <- incrMat(G, multEigen(tmp, H[ind, ] <- crossprodEigen5(tmp, G.old)))
    } else {
      G <- incrMat(G, tmp %*% {H[ind, ] <- crossprod(tmp, G.old)})
    }
  }

  list(H = H, G = G / m)
}

################################################################################

BigMult2 <- function(X, mat, ind.train, block.size,
                     vec.center, vec.scale, use.Eigen) {
  res <- matrix(0, length(ind.train), ncol(mat))

  intervals <- CutBySize(ncol(X), block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.train, ind], vec.center[ind], vec.scale[ind])
    if (use.Eigen) {
      res <- incrMat(res, multEigen(tmp, mat[ind, ]))
    } else {
      res <- incrMat(res, tmp %*% mat[ind, ])
    }
  }

  res
}

################################################################################

# BigCrossprod2 <- function(X, mat, block.size,
#                           vec.center, vec.scale,
#                           use.Eigen = TRUE) {
#   m <- ncol(X)
#   res <- matrix(0, m, ncol(mat))
#
#   intervals <- CutBySize(m, block.size)
#   nb.block <- nrow(intervals)
#
#   for (j in 1:nb.block) {
#     ind <- seq2(intervals[j, ])
#     tmp <- scaling(X[, ind], vec.center[ind], vec.scale[ind])
#     if (use.Eigen) {
#       res[ind, ] <- crossprodEigen5(tmp, mat)
#     } else {
#       res[ind, ] <- crossprod(tmp, mat)
#     }
#   }
#
#   res
# }

################################################################################

#' A randomized algorithm for SVD.
#'
#' A randomized algorithm for SVD (or PCA) of a "big.matrix".
#'
#' @inherit bigstatsr-package params
#' @param K Number of PCs to compute. This algorithm shouldn't
#' be used to compute a lot of PCs. Default is `10`.
#' @param I The number of iterations of the algorithm. Default is `10`.
#' @param backingpath If `X` is filebacked and parallelism is used,
#' the path where are stored the files that are backing `X`.
#'
#' @return
#' @export
#'
#' @example examples/example-randomSVD.R
#' @seealso [big_funScaling] [prcomp] [svd]
#' @references Rokhlin, V., Szlam, A., & Tygert, M. (2010).
#' A Randomized Algorithm for Principal Component Analysis.
#' SIAM Journal on Matrix Analysis and Applications, 31(3), 1100–1124.
#' doi:10.1137/080736417
big_randomSVD <- function(X, fun.scaling = fun_scale,
                          ind.train = seq(nrow(X)),
                          block.size = 1e3,
                          K = 10, I = 10,
                          use.Eigen = TRUE,
                          ncores = 1,
                          backingpath = NULL) {
  check_X(X)
  stopifnot((ncol(X) - K) >= ((I + 1) * (K + 12)))

  if (ncores > 1) {
    ParallelRandomSVD(X, fun.scaling, ind.train, block.size,
                      K, I, use.Eigen, backingpath, ncores)
  } else {
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
                    use.Eigen = use.Eigen)
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
  }
}

### mini test:
# H <- list()
# l <- list(a = matrix(1:4, 2), b = matrix(5:8, 2))
# H[1] <- l["a"]
# l <- list(a = matrix(11:14, 2), b = matrix(5:8, 2))
# H[2] <- l["a"]

