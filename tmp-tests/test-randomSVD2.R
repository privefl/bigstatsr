#' Title
#'
#' @param X
#' @param fun.scaling
#' @param ind.train
#' @param block.size
#' @param K
#' @param I
#' @param extra.K
#' @param use.Eigen
#' @param backingpath
#' @param ncores
#'
#' @return
#' @export
#'
#' @examples
ParallelRandomSVD2.2 <- function(X, fun.scaling,
                                 ind.train = seq(nrow(X)),
                                 block.size = 1000,
                                 K = 10,
                                 I = 5,
                                 extra.K = 50,
                                 use.Eigen = !detect_MRO(),
                                 backingpath = NULL,
                                 ncores = 2,
                                 tol = 1e-3,
                                 maxiter = 10) {
  # parameters
  L <- K + extra.K
  n <- length(ind.train)
  m <- ncol(X)
  stopifnot((n - K) >= (I * L))
  stopifnot(ncores > 1)

  TIME <- 0.01
  tmp.lock.name <- "mutex"
  tmp.lock.names <- paste(tmp.lock.name, Sys.getpid(), 1:4, sep = '-')
  ifelse(file.exists(tmp.lock.names), FALSE,
         file.create(tmp.lock.names))

  # shared big.matrices
  G <- big.matrix(n, L * (I + 1), type = "double", shared = TRUE, init = 0)
  G[, 1:L] <- stats::rnorm(n * L) # G0
  R <- big.matrix(m, L * I, type = "double", shared = TRUE)
  U1 <- big.matrix(n, L * (I + 1), type = "double", shared = TRUE)
  U2 <- big.matrix(m, L * I, type = "double", shared = TRUE)
  T1.t <- big.matrix(m, L * (I + 1), type = "double", shared = TRUE)
  T2.t <- big.matrix(n, L * I, type = "double", shared = TRUE, init = 0)
  remains <- big.matrix(4, I, type = "double", shared = TRUE, init = ncores)
  conv <- big.matrix(2, 1, type = "double", shared = TRUE, init = 0)

  # descriptors
  X.desc <- describe(X)
  G.desc <- describe(G)
  R.desc <- describe(R)
  U1.desc <- describe(U1)
  U2.desc <- describe(U2)
  T1.t.desc <- describe(T1.t)
  T2.t.desc <- describe(T2.t)
  r.desc <- describe(remains)
  conv.desc <- describe(conv)


  intervals <- CutBySize(m, nb = ncores)

  cl <- parallel::makeCluster(ncores, outfile = "")
  doParallel::registerDoParallel(cl)
  scaling <- foreach(ic = seq_len(ncores), .combine = 'cbind') %dopar% {
    lims <- intervals[ic, ]

    # https://www.r-bloggers.com/too-much-parallelism-is-as-bad/
    MRO <- detect_MRO()
    if (MRO) nthreads.save <- RevoUtilsMath::setMKLthreads(1)

    # get big.matrices
    X.part <- sub.big.matrix(X.desc,
                             firstCol = lims[1],
                             lastCol = lims[2],
                             backingpath = backingpath)

    G <- attach.big.matrix(G.desc)
    R <- attach.big.matrix(R.desc)
    R.part <- sub.big.matrix(R.desc, firstRow = lims[1], lastRow = lims[2])

    U1 <- attach.big.matrix(U1.desc)
    U2 <- attach.big.matrix(U2.desc)
    U2.part <- sub.big.matrix(U2.desc, firstRow = lims[1], lastRow = lims[2])

    T1.t <- attach.big.matrix(T1.t.desc)
    T1.t.part <- sub.big.matrix(T1.t.desc, firstRow = lims[1], lastRow = lims[2])
    T2.t <- attach.big.matrix(T2.t.desc)

    remains <- attach.big.matrix(r.desc)
    conv <- attach.big.matrix(conv.desc)

    # scaling
    means_sds <- fun.scaling(X.part, ind.train)
    means <- means_sds$mean
    sds <- means_sds$sd
    rm(means_sds)

    # parameters
    m.part <- ncol(X.part)
    intervals <- CutBySize(m.part, block.size)
    nb.block <- nrow(intervals)

    # temporary matrix
    old.G <- matrix(0, n, L)
    tmp.G <- matrix(0, n, L) # new.G.part
    tmp.R <- matrix(0, m.part, L)
    tmp.T1 <- matrix(0, m.part, L * (I + 1))
    tmp.T2 <- matrix(0, n, L * I)

    it <- 0
    repeat {
      print(it <- it + 1)
      print("OK0")
      print(remains[,])

      # computation of G
      offset <- 0
      for (i in 1:I) {
        # get old G
        old.G[] <- G[, 1:L + offset]
        tmp.G[] <- 0

        for (j in 1:nb.block) {
          ind <- seq2(intervals[j, ])
          tmp <- scaling(X.part[ind.train, ind], means[ind], sds[ind])

          tmp.R[ind, ] <- cross(tmp, old.G, use.Eigen)
          tmp.G <- incrMat(tmp.G, mult(tmp, tmp.R[ind, ], use.Eigen))
        }
        R.part[, 1:L + offset] <- tmp.R
        offset <- offset + L

        print("OK1")

        # increment new G, safely
        file.lock2 <- flock::lock(tmp.lock.names[2])
        incrG(G@address, tmp.G, n, offset, L, m)
        remains[2, i] <- remains[2, i] - 1
        flock::unlock(file.lock2)
        # wait for others at barrier
        while (remains[2, i] > 0) Sys.sleep(TIME)
      }
      print("OK2")
      remains[4, ] <- ncores

      # compute svd(G) once
      file.lock3 <- flock::lock(tmp.lock.names[3])
      if (remains[3, 1] == 1) {
        if (MRO) RevoUtilsMath::setMKLthreads(nthreads.save)
        U1[] <- svd(G[,], nv = 0)$u
        U2[] <- svd(R[,], nv = 0)$u
        if (MRO) RevoUtilsMath::setMKLthreads(1)
      }
      remains[3, 1] <- remains[3, 1] - 1
      flock::unlock(file.lock3)
      # wait for others at barrier
      while (remains[3, 1] > 0) Sys.sleep(TIME)

      print("OK3")

      # compute transpose(T)
      tmp.T2[] <- 0
      for (j in 1:nb.block) {
        ind <- seq2(intervals[j, ])
        tmp <- scaling(X.part[ind.train, ind], means[ind], sds[ind])

        tmp.T1[ind, ] <- cross(tmp, U1[,], use.Eigen)
        tmp.T2 <- incrMat(tmp.T2, mult(tmp, U2.part[ind, ], use.Eigen))
      }
      T1.t.part[] <- tmp.T1

      # increment T2.t, safely
      file.lock1 <- flock::lock(tmp.lock.names[1])
      incrG(T2.t@address, tmp.T2, n, 0, I * L, 1)
      remains[1, 1] <- remains[1, 1] - 1
      flock::unlock(file.lock1)
      # wait for others at barrier
      while (remains[1, 1] > 0) Sys.sleep(TIME)

      print("OK4")

      # compute SVD(T.t), once
      file.lock4 <- flock::lock(tmp.lock.names[4])
      if (remains[4, 1] == 1) {
        if (MRO) RevoUtilsMath::setMKLthreads(nthreads.save)
        str(T1.svd <- svd(T1.t[,], nu = L, nv = L))
        str(T2.svd <- svd(T2.t[,], nu = L, nv = L))
        str(u1 <- mult(U1[,], T1.svd$v, use.Eigen))
        str(v1 <- T1.svd$u)
        str(v2 <- mult(U2[,], T2.svd$v, use.Eigen))
        str(u2 <- T2.svd$u)
        print(diff1 <- diffPCs(u1[, 1:K], u2))
        print(diff2 <- diffPCs(v1[, 1:K], v2))
        print(m1 <- max(diff1, diff2))
        if (m1 < tol) {
          conv[1] <- 1
          conv[2] <- it
          # convention of return
          U1[, 1:K] <- u1[, 1:K]
          T1.t[, 1:K] <- v1[, 1:K]
          U2[1:K, 1] <- T1.svd$d[1:K]
        } else { # here we go again
          G[] <- 0
          G[, 1:L] <- u1
          remains[-4, ] <- ncores
          T2.t[] <- 0
        }
        if (MRO) RevoUtilsMath::setMKLthreads(1)
      }
      remains[4, 1] <- remains[4, 1] - 1
      print(remains[,])
      flock::unlock(file.lock4)
      # wait for others at barrier
      while (remains[4, 1] > 0) Sys.sleep(TIME)
      printf("Conv: (%s, %s)\n", conv[1], conv[2])
      printf("It: %s\n", it)

      print(cond <- conv[1] == 1 || it >= maxiter)
      if (cond) break
    }

    if (MRO) RevoUtilsMath::setMKLthreads(nthreads.save)

    rbind(means, sds)
  }
  parallel::stopCluster(cl)

  # delete temporary lock files
  unlink(tmp.lock.names)

  # convention of return
  list(niter = conv[2], d = U2[1:K, 1], u = U1[, 1:K], v = T1.t[, 1:K],
       means = scaling[1, ], sds = scaling[2, ])
}
