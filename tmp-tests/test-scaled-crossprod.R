mat <- matrix(0, 97, 17); mat[] <- rnorm(length(mat))
n <- nrow(mat)

tmp <- scale(mat)

mu <- attr(tmp, "scaled:center")
delta <- attr(tmp, "scaled:scale")
co <- colSums(mat)

true <- crossprod(tmp)

K <- crossprod(mat)
test <- (K - tcrossprod(co, mu) - tcrossprod(mu, co) + n * tcrossprod(mu)) /
  tcrossprod(delta)

all.equal(test, true)
