################################################################################

BigCrossprod <- function(X, mat, block.size, use.Eigen = TRUE) {
  m <- ncol(X)
  res <- matrix(0, m, ncol(mat))

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    if (use.Eigen) {
      res[ind, ] <- crossprodEigen5(X[, ind], mat)
    } else {
      res[ind, ] <- crossprod(X[, ind], mat)
    }
  }

  res
}

BigCrossprod2 <- function(X, mat, block.size,
                          vec.center, vec.scale,
                          use.Eigen = TRUE) {
  m <- ncol(X)
  res <- matrix(0, m, ncol(mat))

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    tmp <- scaling(X[, ind], vec.center[ind], vec.scale[ind])
    if (use.Eigen) {
      res[ind, ] <- crossprodEigen5(tmp, mat)
    } else {
      res[ind, ] <- crossprod(tmp, mat)
    }
  }

  res
}

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

BigMult <- function(X, mat, block.size, use.Eigen = TRUE) {
  res <- matrix(0, nrow(X), ncol(mat))

  intervals <- CutBySize(ncol(X), block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    if (use.Eigen) { # comparer si vraiment plus rapide
      res <- incrMat(res, multEigen(X[, ind], mat[ind, ]))
    } else {
      res <- incrMat(res, X[, ind] %*% mat[ind, ])
    }
  }

  res
}

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

BigCrossprodSelf <- function(X, block.size2, use.Eigen = TRUE) {
  m <- ncol(X)
  res <- matrix(0, m, m)

  # function to compute X^T*X
  intervals <- CutBySize(m, block.size2)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind1 <- seq2(intervals[j, ])
    tmp1 <- X[, ind1]
    for (i in j:nb.block) {
      ind2 <- seq2(intervals[i, ])
      tmp2 <- X[, ind2]
      if (use.Eigen) {
        res[ind2, ind1] <- crossprodEigen5(tmp2, tmp1)
      } else {
        res[ind2, ind1] <- crossprod(tmp2, tmp1)
      }
    }
  }

  res
}

BigCrossprodSelf2 <- function(X, block.size2,
                             vec.center, vec.scale,
                             use.Eigen = TRUE) {
  m <- ncol(X)
  res <- matrix(0, m, m)

  # function to compute X^T*X
  intervals <- CutBySize(m, block.size2)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind1 <- seq2(intervals[j, ])
    tmp1 <- scaling(X[, ind1], vec.center[ind1], vec.scale[ind1])
    for (i in j:nb.block) {
      ind2 <- seq2(intervals[i, ])
      tmp2 <- scaling(X[, ind2], vec.center[ind2], vec.scale[ind2])
      if (use.Eigen) {
        res[ind2, ind1] <- crossprodEigen5(tmp2, tmp1)
      } else {
        res[ind2, ind1] <- crossprod(tmp2, tmp1)
      }
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
#'
#' @return
#' @export
#'
#' @examples
RandomProjPCA <- function(X, block.size, K = 10, I = 10,
                          use.Eigen = TRUE) {
  check_X(X)

  t1 <- proc.time()

  # parameters
  L <- 2 * K
  n <- nrow(X)
  m <- ncol(X)
  I <- I + 1

  # scaling
  means <- colmeans(X)
  p <- means / 2
  sds <- sqrt(2 * p * (1 - p))

  t2 <- proc.time()
  printf("Computing means took %s seconds\n", (t2 - t1)[3])

  # computation of G and H
  H <- list()
  tmp <- list()
  tmp$G <- matrix(rnorm(n * L), n, L) # G0
  for (i in 1:I) {
    print(i)
    # tmp.H <- BigCrossprod2(X, G, block.size, means, sds,
    #                        use.Eigen = use.Eigen)
    tmp <- getHnG(X, tmp$G, block.size, means, sds,
                  use.Eigen = use.Eigen)
    H[i] <- tmp['H']
    # if (i < I) G <- BigMult2(X, tmp.H, block.size, means, sds,
    #                          use.Eigen = use.Eigen) / m
  }
  rm(tmp)

  t3 <- proc.time()
  printf("Computing H took %s seconds\n", (t3 - t2)[3])

  # svds
  H.svd <- svd(do.call(cbind, H), nv = 0) # m * L * I
  rm(H); gc()

  t4 <- proc.time()
  printf("Computing svd(H) took %s seconds\n", (t4 - t3)[3])

  T.t <- BigMult2(X, H.svd$u, block.size, means, sds,
                  use.Eigen = use.Eigen)
  rm(H.svd)
  T.svd <- svd(T.t, nv = 0)

  t6 <- proc.time()
  printf("Computing T and its svd took %s seconds\n", (t6 - t4)[3])

  list(d = T.svd$d[1:K], u = T.svd$u[, 1:K, drop = FALSE])
}

### mini test:
# H <- list()
# l <- list(a = matrix(1:4, 2), b = matrix(5:8, 2))
# H[1] <- l["a"]
# l <- list(a = matrix(11:14, 2), b = matrix(5:8, 2))
# H[2] <- l["a"]
