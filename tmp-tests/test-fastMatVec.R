# scaling by matrices
X <- matrix(0, 43, 57)
X[] <- rnorm(length(X))

X2 <- scale(X)

n <- nrow(X)
sd <- apply(X, 2, sd)
X3 <- (diag(n) - tcrossprod(rep(1, n)) / n) %*% X %*% diag(1 / sd)

print(max(abs(X2 - X3)))

# test fast product of mat-vec

require(bigsnpr)
require(bigstatsr)

popres <- AttachBigSNP("../thesis-celiac/popres/backingfiles/popres_sub1.bk")
X <- popres$genotypes
print(dim(X))

# X2 <- deepcopy(X, backingfile = "tmp", backingpath = ".")
X2 <- attach.big.matrix("tmp.desc")
y <- rnorm(ncol(X2))

print(system.time(
  test <- produ(X2@address, y)
)) # super fast and memory efficient
# test2 <- X2[,] %*% y # 6 Go

scal <- snp_scaleBinom(X2)
test <- produ(X2@address, y / scal$sd)
A <- function(x, args, scal) {
  x <- x / scal$sd
  tmp <- produ(args@address, x)
  tmp - sum(x * scal$mean)
}
test <- A(y, X2, scal)
print(mean(test))

Atrans <- function(x, args, scal) {
  tmp <- crossprodu(args@address, x)
  (tmp - sum(x) * scal$mean) / scal$sd
}
y2 <- rnorm(nrow(X2))
test2 <- Atrans(y2, X2, scal)

require(RSpectra)
svds2 <- function(X, fun.scaling, k = 10, tol = 1e-4) {
  scal <- fun.scaling(X)
  A <- function(x, args) {
    x <- x / scal$sd
    tmp <- produ(args@address, x)
    tmp - sum(x * scal$mean)
  }
  Atrans <- function(x, args) {
    tmp <- crossprodu(args@address, x)
    (tmp - sum(x) * scal$mean) / scal$sd
  }
  svds(A, k, nu = k, nv = k, opts = list(tol = tol),
       Atrans = Atrans, dim = dim(X), args = X)
}

print(system.time(
  test <- svds2(X2, snp_scaleBinom)
))
# tol = 1e-10 -> 431 sec / 17 / 262
# tol = 1e-4  -> 276 sec / 10 / 166 -> sufficient
# ncv = 50    -> 284 sec /  2 / 172 -> ncv is useless

true <- big_SVD(X2, snp_scaleBinom, k = 10)
print(all.equal(true$d, test$d))
plot(true$u, test$u)
s <- c(rep(FALSE, 19), TRUE)
plot(true$v[s], test$v[s])
print(diffPCs(test$u, true$u))
print(diffPCs(test$v, true$v))
