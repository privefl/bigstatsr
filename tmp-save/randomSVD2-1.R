################################################################################

getHnG <- function(X, G1.old, G2.old, ind.train, block.size,
                   vec.center, vec.scale, use.Eigen) {
  n <- length(ind.train)
  m <- ncol(X)
  m2 <- ncol(G1.old)
  H1 <- matrix(0, m, m2)
  H2 <- matrix(0, m, m2)
  G1 <- matrix(0, n, m2)
  G2 <- matrix(0, n, m2)

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[ind.train, ind], vec.center[ind], vec.scale[ind])
    if (use.Eigen) {
      G1 <- incrMat(G1, multEigen(tmp, H1[ind, ] <-
                                    crossprodEigen5(tmp, G1.old)))
      G2 <- incrMat(G2, multEigen(tmp, H2[ind, ] <-
                                    crossprodEigen5(tmp, G2.old)))
    } else {
      G1 <- incrMat(G1, tmp %*% {H1[ind, ] <- crossprod(tmp, G1.old)})
      G2 <- incrMat(G2, tmp %*% {H2[ind, ] <- crossprod(tmp, G2.old)})
    }
  }

  list(H1 = H1, H2 = H2, G1 = G1 / (2*m), G2 = G2 / (2*m))
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
big_randomSVD2 <- function(X, fun.scaling,
                           ind.train = seq(nrow(X)),
                           block.size = 1e3,
                           K = 10, I = 10,
                           use.Eigen = TRUE,
                           TOL = 1e-7) {
  check_X(X)
  stopifnot((ncol(X) - K) >= ((I + 1) * (K + 12)))


  # parameters
  n <- length(ind.train)
  m <- ncol(X)
  I <- 10
  L <- K + 12

  # scaling
  stats <- fun.scaling(X, ind.train)
  means <- stats$mean
  sds <- stats$sd
  rm(stats)

  diffPCs <- function(test, rot) {
    k <- ncol(test)
    diff1 <- 2 * abs(test - rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
    diff2 <- 2 * abs(test + rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
    diff <- pmin(diff1, diff2)
    mean(diff)
  }

  # computation of G and H
  H1 <- list()
  H2 <- list()
  tmp <- list(G1 = matrix(rnorm(n * L), n, L),
              G2 = matrix(rnorm(n * L), n, L)) # G0

  it <- 0
  old.diff <- Inf
  repeat {
    print(it)
    for (i in 1:I + it*I) {
      tmp <- getHnG(X, tmp$G1, tmp$G2, ind.train, block.size, means, sds,
                    use.Eigen = use.Eigen)
      H1[i] <- tmp['H1']
      H2[i] <- tmp['H2']
    }

    # svds
    H1.u <- svd(do.call(cbind, H1), nv = 0)$u # m * L * I
    H2.u <- svd(do.call(cbind, H2), nv = 0)$u # m * L * I

    T1.t <- BigMult2(X, H1.u, ind.train, block.size, means, sds,
                     use.Eigen = use.Eigen)
    T2.t <- BigMult2(X, H2.u, ind.train, block.size, means, sds,
                     use.Eigen = use.Eigen)
    T1.svd <- svd(T1.t, nu = K, nv = K)
    T2.svd <- svd(T2.t, nu = K, nv = K)

    print(new.diff <- diffPCs(T1.svd$u, T2.svd$u))
    if(new.diff < TOL || old.diff < (1.5 * new.diff)) break
    it <- it + 1
    old.diff <- new.diff
  }

  list(d = T1.svd$d[1:K], u = T1.svd$u, v = H1.u %*% T1.svd$v,
       means = means, sds = sds, diff = new.diff)
}

### mini test:
# H <- list()
# l <- list(a = matrix(1:4, 2), b = matrix(5:8, 2))
# H[1] <- l["a"]
# l <- list(a = matrix(11:14, 2), b = matrix(5:8, 2))
# H[2] <- l["a"]

