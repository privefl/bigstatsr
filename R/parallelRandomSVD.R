ParallelRandomSVD <- function(X, fun.scaling,
                              ind.train,
                              block.size,
                              K, I,
                              use.Eigen,
                              backingpath,
                              ncores) {
  # parameters
  tmp.lock.name <- "mutex"
  TIME <- 0.01
  L <- K + 12
  n <- length(ind.train)
  m <- ncol(X)
  I <- I + 1
  tmp.lock.names <- paste0(tmp.lock.name, 1:4)
  ifelse(file.exists(tmp.lock.names), FALSE,
         file.create(tmp.lock.names))

  # shared big.matrices
  G <- big.matrix(n, L, type = "double", shared = TRUE)
  G[] <- rnorm(n * L) # G0
  H <- big.matrix(m, L * I, type = "double", shared = TRUE)
  U <- big.matrix(m, L * I, type = "double", shared = TRUE)
  T.t <- big.matrix(n, L * I, type = "double", shared = TRUE, init = 0)
  remains <- big.matrix(4, I - 1, type = "integer",
                        shared = TRUE, init = ncores)

  # descriptors
  X.desc <- describe(X)
  G.desc <- describe(G)
  H.desc <- describe(H)
  U.desc <- describe(U)
  T.t.desc <- describe(T.t)
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
    remains <- attach.big.matrix(r.desc)

    # scaling
    means_sds <- FUN(X.part, ind.train)
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
        tmp <- scaling(X.part[ind.train, ind], means[ind], sds[ind])

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
      tmp <- scaling(X.part[ind.train, ind], means[ind], sds[ind])

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
      tmp <- scaling(X.part[ind.train, ind], means[ind], sds[ind])

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

    rbind(means, sds)
  }

  intervals <- CutBySize(m, nb = ncores)
  obj <- foreach::foreach(i = 1:ncores, .combine = 'cbind',
                          .packages = "bigmemory")
  expr_fun <- function(i) part(intervals[i, ])

  scaling <- foreach2(obj, expr_fun, ncores)

  # delete temporary lock files
  unlink(tmp.lock.names)

  T.svd <- svd(T.t[,], nu = K, nv = K)

  list(d = T.svd$d[1:K], u = T.svd$u, v = U[,] %*% T.svd$v,
       means = scaling[1, ], sds = scaling[2, ])
}
