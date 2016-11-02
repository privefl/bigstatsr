#' Title
#'
#' @param X
#' @param fun.scaling
#' @param block.size
#' @param K
#' @param I
#' @param use.Eigen
#' @param backingpath
#' @param tmp.lock.name
#' @param TIME
#' @param ncores
#'
#' @return
#' @export
#'
#' @examples
ParallelRandomProjPCA <- function(X, fun.scaling,
                                  block.size = 1e3,
                                  K = 10, I = 10,
                                  use.Eigen = TRUE,
                                  backingpath = NULL,
                                  tmp.lock.name = "file.lock",
                                  TIME = 0.01,
                                  ncores) {
  check_X(X)

  # parameters
  L <- 2 * K
  n <- nrow(X)
  m <- ncol(X)
  I <- I + 1
  tmp.lock.names <- paste0(tmp.lock.name, 1:5)
  ifelse(file.exists(tmp.lock.names), FALSE,
         file.create(tmp.lock.names))

  # shared big.matrices
  G <- big.matrix(n, L, type = "double", shared = TRUE)
  G[] <- rnorm(n * L) # G0
  H <- big.matrix(m, L * I, type = "double", shared = TRUE)
  U <- big.matrix(m, L * I, type = "double", shared = TRUE)
  T.t <- big.matrix(n, L * I, type = "double", shared = TRUE, init = 0)
  U2 <- big.matrix(n, K, type = "double", shared = TRUE)
  D <- big.matrix(K, K, type = "double", shared = TRUE, init = 0)
  V <- big.matrix(m, K, type = "double", shared = TRUE)
  remains <- big.matrix(5, I - 1, type = "integer",
                        init = ncores, shared = TRUE)
  # descriptors
  X.desc <- describe(X)
  G.desc <- describe(G)
  H.desc <- describe(H)
  U.desc <- describe(U)
  T.t.desc <- describe(T.t)
  U2.desc <- describe(U2)
  D.desc <- describe(D)
  V.desc <- describe(V)
  r.desc <- describe(remains)

  # export function
  FUN <- fun.scaling

  part <- function(lims) {
    # get big.matrices
    X.part <- sub.big.matrix(X.desc,
                             firstCol = lims[1],
                             lastCol = lims[2],
                             backingpath = backingpath)
    G <- attach.big.matrix(G.desc)
    H <- attach.big.matrix(H.desc)
    U <- attach.big.matrix(U.desc)
    T.t <- attach.big.matrix(T.t.desc)
    U2 <- attach.big.matrix(U2.desc)
    D <- attach.big.matrix(D.desc)
    V <- attach.big.matrix(V.desc)
    remains <- attach.big.matrix(r.desc)

    # scaling
    means_sds <- FUN(X.part)
    means <- means_sds$mean
    sds <- means_sds$sd
    rm(means_sds)

    # parameters
    ind.part <- seq2(lims)
    m.part <- ncol(X.part)
    intervals <- CutBySize(m.part, block.size)
    nb.block <- nrow(intervals)

    # computation of G and H
    for (i in 1:(I - 1)) {
      # get G, safely
      old.G <- G[,]
      file.lock1 <- flock::lock(tmp.lock.names[1])
      if (remains[1, i] == 1) G[] <- 0 # init new G with 0s
      remains[1, i] <- remains[1, i] - 1L
      flock::unlock(file.lock1)

      tmp.G <- matrix(0, n, L)
      for (j in 1:nb.block) {
        ind <- seq2(intervals[j, ])
        tmp <- scaling(X.part[, ind], means[ind], sds[ind])

        if (use.Eigen) {
          tmp.H <- crossprodEigen5(tmp, old.G)
          tmp.G <- incrMat(tmp.G, multEigen(tmp, tmp.H))
        } else {
          tmp.H <- crossprod(tmp, old.G)
          tmp.G <- incrMat(tmp.G, tmp %*% tmp.H)
        }

        H[ind.part[ind], 1:L + (i - 1) * L] <- tmp.H
      }

      # wait for others at barrier
      while (remains[1, i] > 0) Sys.sleep(TIME)
      # increment G, safely
      file.lock2 <- flock::lock(tmp.lock.names[2])
      incrG(G@address, tmp.G, n, L, m)
      remains[2, i] <- remains[2, i] - 1L
      flock::unlock(file.lock2)
      # wait for others at barrier
      while (remains[2, i] > 0) Sys.sleep(TIME)
    }

    i <- I # no need G_{I + 1}
    old.G <- G[,]
    for (j in 1:nb.block) {
      ind <- seq2(intervals[j, ])
      tmp <- scaling(X.part[, ind], means[ind], sds[ind])

      if (use.Eigen) {
        tmp.H <- crossprodEigen5(tmp, old.G)
      } else {
        tmp.H <- crossprod(tmp, old.G)
      }

      H[ind.part[ind], 1:L + (i - 1) * L] <- tmp.H
    }

    # compute svd(H) once
    file.lock3 <- flock::lock(tmp.lock.names[3])
    if (remains[3, 1] == 1) U[] <- svd(H[,], nv = 0)$u
    remains[3, 1] <- remains[3, 1] - 1L
    flock::unlock(file.lock3)
    # wait for others at barrier
    while (remains[3, 1] > 0) Sys.sleep(TIME)

    # compute transpose(T)
    tmp.T.t <- matrix(0, n, I * L)
    for (j in 1:nb.block) {
      ind <- seq2(intervals[j, ])
      ind2 <- ind.part[ind]
      tmp <- scaling(X.part[, ind], means[ind], sds[ind])

      if (use.Eigen) {
        tmp.T.t <- incrMat(tmp.T.t, multEigen(tmp, U[ind2, ]))
      } else {
        tmp.T.t <- incrMat(tmp.T.t, tmp %*% U[ind2, ])
      }
    }
    # increment T.t, safely
    file.lock4 <- flock::lock(tmp.lock.names[4])
    incrG(T.t@address, tmp.T.t, n, I * L, 1)
    remains[4, 1] <- remains[4, 1] - 1L
    flock::unlock(file.lock4)
    # wait for others at barrier
    while (remains[4, 1] > 0) Sys.sleep(TIME)

    # compute svd(T.t) once
    file.lock5 <- flock::lock(tmp.lock.names[5])
    if (remains[5, 1] == 1) {
      T.t.svd <- svd(T.t[,], nu = K, nv = 0)
      U2[] <- T.t.svd$u
      diag(D) <- T.t.svd$d[1:K]
    }
    remains[5, 1] <- remains[5, 1] - 1L
    flock::unlock(file.lock5)
    # wait for others at barrier
    while (remains[5, 1] > 0) Sys.sleep(TIME)

    # compute V
    d <- diag(D[,])
    ud <- U2[,] %*% diag(1 / d)
    for (j in 1:nb.block) {
      ind <- seq2(intervals[j, ])
      tmp <- scaling(X.part[, ind], means[ind], sds[ind])

      if (use.Eigen) {
        V[ind.part[ind], ] <- crossprodEigen5(tmp, ud)
      } else {
        V[ind.part[ind], ] <- crossprod(tmp, ud)
      }
    }

    0
  }

  intervals <- CutBySize(m, nb = ncores)
  obj <- foreach::foreach(i = 1:ncores, .packages = "bigmemory")
  expr_fun <- function(i) part(intervals[i, ])

  foreach2(obj, expr_fun, ncores)

  # delete temporary lock files
  unlink(tmp.lock.names)

  list(d = diag(D[,]), u = U2[,], v = V[,])
}
