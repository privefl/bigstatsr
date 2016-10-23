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

  # scaling
  p <- colmeans(X) / 2
  sd <- sqrt(2 * p * (1 - p))
  Y <- deepcopy(X, type = "double", shared = FALSE)
  scaled(Y@address, 2 * p, sd)
  rm(p, sd)

  # parameters
  L <- 2 * K
  m <- ncol(Y)
  n <- nrow(Y)
  I <- I + 1

  # computation of G and H
  H <- big.matrix(m, I * L, type = "double", shared = FALSE)
  G <- matrix(rnorm(n * L), n, L) # G0
  for (i in 1:I) {
    tmp.H <- BigCrossprod(Y, G, block.size,
                          use.Eigen = use.Eigen)
    H[, 1:L + (i - 1) * L] <- tmp.H
    if (i < I) G <- BigMult(Y, tmp.H, block.size,
                            use.Eigen = use.Eigen) / m
  }
  rm(G, tmp.H)

  # svds
  H.svd <- svd(H[,]) # m * L * I

  check_K <- function() {
    block.size2 <- max(1, floor(n / m * block.size))
    K.H <- BigCrossprodSelf(H, block.size2)
    K.H.eigs <- eigen(K.H, symmetric = TRUE)
    for (i in 1:K) {
      diff1 <- abs(K.H.eigs$vectors[, i] - H.svd$v[, i])
      diff2 <- abs(K.H.eigs$vectors[, i] + H.svd$v[, i])
      diff <- pmin(diff1, diff2)
      if (!isTRUE(all.equal(max(diff), 0))) {
        printf("You could take K = %d\n", i - 1)
        break
      }
    }
  }
  check_K()
  rm(H); gc()

  T.t <- BigMult(Y, H.svd$u, block.size)
  rm(H.svd)
  T.svd <- svd(T.t, nv = 0)
  sweep(T.svd$u[, 1:K], 2, (T.svd$d)[1:K], '*')
}
