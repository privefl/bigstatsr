################################################################################

getHnG <- function(X, G.old, block.size,
                   vec.center, vec.scale,
                   use.Eigen = TRUE) {
  n <- nrow(X)
  m <- ncol(X)
  m2 <- ncol(G.old)
  H <- matrix(0, m, m2)
  G <- matrix(0, n, m2)

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[, ind], vec.center[ind], vec.scale[ind])
    if (use.Eigen) {
      G <- incrMat(G, multEigen(tmp, H[ind, ] <- crossprodEigen5(tmp, G.old)))
    } else {
      G <- incrMat(G, tmp %*% {H[ind, ] <- crossprod(tmp, G.old)})
    }
  }

  list(H = H, G = G / m)
}

################################################################################

BigMult2 <- function(X, mat, block.size,
                     vec.center, vec.scale,
                     use.Eigen = TRUE) {
  res <- matrix(0, nrow(X), ncol(mat))

  intervals <- CutBySize(ncol(X), block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[, ind], vec.center[ind], vec.scale[ind])
    if (use.Eigen) { # comparer si vraiment plus rapide
      res <- incrMat(res, multEigen(tmp, mat[ind, ]))
    } else {
      res <- incrMat(res, tmp %*% mat[ind, ])
    }
  }

  res
}

################################################################################

#' Title
#'
#' @param X
#' @param block.size
#' @param K
#' @param I
#' @param use.Eigen
#' @param fun.scaling
#'
#' @return
#' @export
#'
#' @examples
RandomProjPCA <- function(X, fun.scaling,
                          block.size = 1e3,
                          K = 10, I = 10,
                          use.Eigen = TRUE) {
  check_X(X)

  # parameters
  L <- 2 * K
  n <- nrow(X)
  m <- ncol(X)
  I <- I + 1

  # scaling
  stats <- fun.scaling(X)
  means <- stats$mean
  sds <- stats$sd
  rm(stats)

  # computation of G and H
  H <- list()
  tmp <- list()
  tmp$G <- matrix(rnorm(n * L), n, L) # G0
  for (i in 1:I) {
    tmp <- getHnG(X, tmp$G, block.size, means, sds,
                  use.Eigen = use.Eigen)
    H[i] <- tmp['H']
  }
  rm(tmp)

  # svds
  H.svd <- svd(do.call(cbind, H), nv = 0) # m * L * I
  rm(H); gc()

  T.t <- BigMult2(X, H.svd$u, block.size, means, sds,
                  use.Eigen = use.Eigen)
  rm(H.svd)
  T.svd <- svd(T.t, nv = 0)

  list(d = T.svd$d[1:K], u = T.svd$u[, 1:K, drop = FALSE])
}

### mini test:
# H <- list()
# l <- list(a = matrix(1:4, 2), b = matrix(5:8, 2))
# H[1] <- l["a"]
# l <- list(a = matrix(11:14, 2), b = matrix(5:8, 2))
# H[2] <- l["a"]
