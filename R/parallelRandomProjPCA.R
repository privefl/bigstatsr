#' Title
#'
#' @param X
#' @param block.size
#' @param ncores
#' @param K
#' @param I
#' @param backingpath
#' @param tmp.lock.name
#' @param TIME
#'
#' @return
#' @export
#'
#' @examples
ParallelRandomProjPCA <- function(X, block.size, ncores,
                                  K = 10, I = 10,
                                  backingpath = NULL,
                                  tmp.lock.name = "file.lock",
                                  TIME = 0.01) {
  check_X(X)

  # parameters
  L <- 2 * K
  n <- nrow(X)
  m <- ncol(X)
  I <- I + 1
  tmp.lock.names <- paste0(tmp.lock.name, 1:2)
  ifelse(file.exists(tmp.lock.names), FALSE,
         file.create(tmp.lock.names))

  # shared big.matrices
  G <- big.matrix(n, L, type = "double", shared = TRUE)
  G[] <- rnorm(n * L) # G0
  H <- big.matrix(m, L * I, type = "double", shared = TRUE)
  remains <- big.matrix(2, I, type = "integer",
                        init = ncores, shared = TRUE)
  # descriptors
  X.desc <- describe(X)
  H.desc <- describe(H)
  G.desc <- describe(G)
  r.desc <- describe(remains)

  t2 <- proc.time()

  part <- function(lims) {
    # get big.matrices
    X.part <- sub.big.matrix(X.desc,
                             firstCol = lims[1],
                             lastCol = lims[2],
                             backingpath = backingpath)
    G <- attach.big.matrix(G.desc)
    H <- attach.big.matrix(H.desc)
    remains <- attach.big.matrix(r.desc)

    # scaling
    means <- colmeans(X.part)
    p <- means / 2
    sds <- sqrt(2 * p * (1 - p))

    # parameters
    ind.part <- seq2(lims)
    m.part <- ncol(X.part)
    intervals <- CutBySize(m.part, block.size)
    nb.block <- nrow(intervals)

    # computation of G and H
    for (i in 1:I) {
      # get G, safely
      old.G <- G[,]
      file.lock1 <- flock::lock(tmp.lock.names[1])
      remains[1, i] <- remains[1, i] - 1L
      if (remains[1, i] == 0) G[] <- 0 # init new G with 0s
      flock::unlock(file.lock1)
      # wait for others at barrier
      while (remains[1, i] > 0) Sys.sleep(TIME)

      tmp.G <- matrix(0, n, L)
      for (j in 1:nb.block) {
        ind <- seq2(intervals[j, ])
        tmp <- scaling(X.part[, ind], means[ind], sds[ind])
        tmp.H <- crossprodEigen5(tmp, old.G)
        H[ind.part[ind], 1:L + (i - 1) * L] <- tmp.H
        tmp.G <- incrMat(tmp.G, multEigen(tmp, tmp.H))
      }

      # increment G, safely
      file.lock2 <- flock::lock(tmp.lock.names[2])
      incrG(G@address, tmp.G, n, L, m)
      remains[2, i] <- remains[2, i] - 1L
      flock::unlock(file.lock2)
      # wait for others at barrier
      while (remains[2, i] > 0) Sys.sleep(TIME)
    }

    rbind(means, sds)
  }

  intervals <- CutBySize(m, nb = ncores)
  obj <- foreach::foreach(i = 1:nrow(intervals),
                          .combine = 'cbind',
                          .packages = "bigmemory")
  expr_fun <- function(i) part(intervals[i, ])
  tmp <- foreach2(obj, expr_fun, ncores)

  t3 <- proc.time()
  printf("Computing H took %s seconds\n", (t3 - t2)[3])

  # svds
  H.svd <- svd(H[,], nv = 0) # m * L * I
  rm(H); gc()

  t4 <- proc.time()
  printf("Computing svd(H) took %s seconds\n", (t4 - t3)[3])

  T.t <- BigMult2(X, H.svd$u, block.size, tmp[1, ], tmp[2, ])
  rm(H.svd)
  T.svd <- svd(T.t, nv = 0)

  t6 <- proc.time()
  printf("Computing T and its svd took %s seconds\n", (t6 - t4)[3])

  # delete temporary lock files
  unlink(tmp.lock.names)

  list(d = T.svd$d[1:K], u = T.svd$u[, 1:K, drop = FALSE])
}
