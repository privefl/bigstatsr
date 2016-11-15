ParallelRandomSVD2 <- function(X, fun.scaling,
                               ind.train,
                               block.size,
                               K, I,
                               use.Eigen,
                               backingpath,
                               ncores) {
  # parameters
  L <- K + 12
  n <- length(ind.train)
  m <- ncol(X)
  I <- I + 1
  stopifnot((n - K) >= (I * L))

  TIME <- 0.01
  tmp.lock.name <- "mutex"
  tmp.lock.names <- paste(tmp.lock.name, Sys.getpid(), 1:3, sep = '-')
  ifelse(file.exists(tmp.lock.names), FALSE,
         file.create(tmp.lock.names))

  # shared big.matrices
  G <- big.matrix(n, L, type = "double", shared = TRUE)
  G[] <- stats::rnorm(n * L) # G0
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


  intervals <- CutBySize(m, nb = ncores)

  if (is.seq <- (ncores == 1)) {
    registerDoSEQ()
  } else {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
  }
  scaling <- foreach(ic = seq_len(ncores), .combine = 'cbind') %dopar% {
    lims <- intervals[ic, ]

    # get big.matrices
    X.part <- sub.big.matrix(X.desc,
                             firstCol = lims[1],
                             lastCol = lims[2],
                             backingpath = backingpath)

    # scaling
    means_sds <- fun.scaling(X.part, ind.train)
    means <- means_sds$mean
    sds <- means_sds$sd
    rm(means_sds)

    # parameters
    m.part <- ncol(X.part)
    intervals <- CutBySize(m.part, block.size)
    nb.block <- nrow(intervals)

    # computation of G and H
    G <- attach.big.matrix(G.desc)
    H.part <- sub.big.matrix(H.desc, firstRow = lims[1], lastRow = lims[2])
    remains <- attach.big.matrix(r.desc)
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

        H.part[ind, 1:L + (i - 1) * L] <- tmp.H
      }

      # wait for others at barrier
      while (remains[1, i] > 0) Sys.sleep(TIME)
      # increment G, safely
      file.lock2 <- flock::lock(tmp.lock.names[2])
      incrG(G@address, tmp.G, n, L, 2*m)
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

      H.part[ind, 1:L + (i - 1) * L] <- tmp.H
    }
    rm(G, H.part)

    # compute svd(H) once
    file.lock3 <- flock::lock(tmp.lock.names[3])
    if (remains[3, 1] == 1) {
      H <- attach.big.matrix(H.desc)
      U <- attach.big.matrix(U.desc)
      U[] <- svd(H[,], nv = 0)$u
    }
    remains[3, 1] <- remains[3, 1] - 1L
    flock::unlock(file.lock3)
    # wait for others at barrier
    while (remains[3, 1] > 0) Sys.sleep(TIME)

    # compute transpose(T)
    U.part <- sub.big.matrix(U.desc, firstRow = lims[1], lastRow = lims[2])
    tmp.T.t <- matrix(0, n, I * L)
    for (j in 1:nb.block) {
      ind <- seq2(intervals[j, ])
      tmp <- scaling(X.part[ind.train, ind], means[ind], sds[ind])

      if (use.Eigen) {
        tmp.T.t <- incrMat(tmp.T.t, multEigen(tmp, U.part[ind, ]))
      } else {
        tmp.T.t <- incrMat(tmp.T.t, tmp %*% U.part[ind, ])
      }
    }
    rm(U.part)

    # increment T.t, safely
    T.t <- attach.big.matrix(T.t.desc)
    file.lock4 <- flock::lock(tmp.lock.names[4])
    incrG(T.t@address, tmp.T.t, n, I * L, 1)
    remains[4, 1] <- remains[4, 1] - 1L
    flock::unlock(file.lock4)
    # wait for others at barrier
    while (remains[4, 1] > 0) Sys.sleep(TIME)

    rbind(means, sds)
  }
  if (!is.seq) parallel::stopCluster(cl)

  # delete temporary lock files
  unlink(tmp.lock.names)

  T.svd <- svd(T.t[,], nu = K, nv = K)

  list(d = T.svd$d[1:K], u = T.svd$u, v = U[,] %*% T.svd$v,
       means = scaling[1, ], sds = scaling[2, ])
}
