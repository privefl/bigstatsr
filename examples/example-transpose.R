X <- big.matrix(11, 23, shared = FALSE)
X[] <- rnorm(length(X))

Xt <- big_transpose(X, shared = FALSE)

print(identical(
  t(X[,]),
  Xt[,]
))
