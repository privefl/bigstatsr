X <- big.matrix(11, 23, shared = FALSE)
X[] <- rnorm(length(X))

Xt <- transpose(X, shared = FALSE)

print(all.equal(t(X[,]), Xt[,]))


