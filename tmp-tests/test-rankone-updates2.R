# http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.215.3851&rep=rep1&type=pdf

require(bigsnpr)

celiac <- AttachBigSNP("../bigsnpr/backingfiles/celiac_impute1_sub1.bk")
X <- celiac$genotypes
M <- 100
X2 <- sub.big.matrix(X, lastCol = M)
print(dim(X2))
N <- nrow(X)
y <- rnorm(N)

K <- 2
covar <- cbind(1, matrix(rnorm(N*K), N))
covar.svd <- svd(covar)

Rcpp::sourceCpp('tmp-tests/univRegLin4.cpp')
test <- univRegLin4(X2@address, y, covar.svd, 1:N)

K <- K + 1
# a = y = X0
for (j in 1:M) {
  X0 <- X2[, j]
  a <- X0

  b <- c(1, rep(0, K))
  Q <- as.matrix(b)

  mv <- crossprod(covar.svd$u, a)
  pv <- a - covar.svd$u %*% mv
  p <- as.numeric(sqrt(crossprod(pv)))
  P <- pv / p

  pUty <- c(crossprod(P, y), crossprod(covar.svd$u, y))

  V2 <- cbind(Q, rbind(0, covar.svd$v)) # V2 constant
  test1 <- cbind(X0, covar) %*% V2
  U2 <- cbind(P, covar.svd$u) # U2 constant expect first column
  test3 <- crossprod(U2, test1)
  test3[abs(test3) < 1e-12] <- 0
  print(test3)

  true <- cbind(c(p, mv), rbind(0, diag(covar.svd$d)))
  print(all.equal(test3, true))


  # diagonalize test
  X.svd2 <- svd(test3)
  U.final <- U2 %*% X.svd2$u
  V.final <- V2 %*% X.svd2$v

  true2 <- svd(cbind(X0, covar))
  test2 <- list(d = X.svd2$d, u = U.final, v = V.final)
  # plot(true2$u, test2$u)
  # plot(true2$v, test2$v)

  w <- test2$v[1, ] / test2$d
  z <- crossprod(X.svd2$u, pUty)
  tmp <- sum(w^2)
  beta <- crossprod(w, z)
  eps <- y - test2$u %*% z

  std <- sqrt(tmp * sum(eps^2) / (N - K - 1));

  # mylm <- summary(lm(y ~ cbind(X0, covar)-1))
  # coefs <- mylm$coefficients[1, 1:2]
  # print(coefs); print(c(beta, std))
}

