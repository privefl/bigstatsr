# http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.215.3851&rep=rep1&type=pdf

# simulate some data
N <- 50
M <- 5
X <- matrix(rnorm(N*M), N)
X.svd <- svd(X)

# a = y = X0
X0 <- rnorm(N)
y <- X0
a <- X0

b <- c(rep(0, M), 1)

mv <- crossprod(X.svd$u, a)
pv <- a - X.svd$u %*% mv
p <- as.numeric(sqrt(crossprod(pv)))
P <- pv / p

Q <- as.matrix(b)

V2 <- cbind(rbind(X.svd$v, 0), Q)
test1 <- cbind(X, X0) %*% V2
# test1[, M+1] <- y
U2 <- cbind(X.svd$u, P)
test <- crossprod(U2, test1)
test[abs(test) < 1e-14] <- 0
print(test)

true <- cbind(rbind(diag(X.svd$d), 0), c(mv, p))
print(all.equal(test, true))


# diagonalize test
X.svd2 <- svd(test)
U.final <- U2 %*% X.svd2$u # U2 constant expect last column
V.final <- V2 %*% X.svd2$v # V2 constant

true2 <- svd(cbind(X, X0))
print(all.equal(list(d = X.svd2$d, u = U.final, v = V.final), true2))
test
