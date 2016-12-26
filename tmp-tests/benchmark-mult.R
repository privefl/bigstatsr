require(bigstatsr)

# X2 <- deepcopy(X, backingfile = "tmp", backingpath = ".")
X2 <- attach.big.matrix("tmp.desc")
m <- ncol(X2)
y <- rnorm(m)

print(system.time(
  tmp <- test(X2@address, y, rowInd = seq(nrow(X2)))
))

print(system.time(
  tmp <- test2(X2@address, y)
))

y2 <- matrix(0, m, 50)
y2[] <- rnorm(length(y2))
print(system.time(
  tmp <- test3(X2@address, y2, rowInd = seq(nrow(X2)))
)) # 80 sec


source('R/utils-mult.R')
source('R/utils.R')
Rcpp::sourceCpp('src/utils-mat.cpp')

print(system.time(
  tmp2 <- multScaled(X2, mat = y2, ind.train = seq(nrow(X2)),
                     block.size = 1000, vec.center = rep(0, m),
                     vec.scale = rep(1, m), use.Eigen = TRUE)
)) # 6 sec -> 8 sec with Eigen
print(all.equal(tmp, tmp2))

big_mult <- function(X, mat, ind.train, block.size, use.Eigen) {
  m <- ncol(X)
  if (!is.matrix(y)) mat <- as.matrix(mat)
  stopifnot(m == nrow(mat))
  res <- matrix(0, length(ind.train), ncol(mat))

  intervals <- CutBySize(m, block.size)
  nb.block <- nrow(intervals)

  for (j in 1:nb.block) {
    ind <- seq2(intervals[j, ])
    res <- incrMat(res, mult(X[ind.train, ind], mat[ind, ], use.Eigen))
  }

  res
}

print(system.time(
  tmp4 <- big_mult(X2, mat = y2, ind.train = seq(nrow(X2)),
                         block.size = 1000, use.Eigen = FALSE)
)) # 4.5 sec
print(all.equal(tmp, tmp4))

big_multScaled <- function(X, mat, ind.train, block.size,
                           vec.center, vec.scale, use.Eigen) {
  mat <- scaling2(mat, vec.scale)
  tmp <- big_mult(X, mat, ind.train, block.size, use.Eigen)
  tmp2 <- crossprod(vec.center, mat)
  scaling3(tmp, as.numeric(tmp2))
}

print(system.time(
  tmp3 <- big_multScaled(X2, mat = y2, ind.train = seq(nrow(X2)),
                     block.size = 1000, vec.center = rep(0, m),
                     vec.scale = rep(1, m), use.Eigen = FALSE)
)) # 5.5 sec
print(all.equal(tmp, tmp3))

print(system.time(
  tmp5 <- big_mult(X2, mat = y, ind.train = seq(nrow(X2)),
                   block.size = 1000, use.Eigen = FALSE)
)) # 3.2 sec

print(system.time(
  tmp5 <- big_mult(X2, mat = cbind(y2, y2), ind.train = seq(nrow(X2)),
                   block.size = 1000, use.Eigen = FALSE)
)) # 6.1 sec
# almost no cost to adding dimensions to the matrix
# for one vector, use eigen
# arma is bad for reading a big.matrix
