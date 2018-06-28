x <- runif(10)
X <- matrix(runif(30), 10, 3)

U <- svd(X)$u
crossprod(U)

X2 <- cbind(x, U)
(K <- crossprod(X2))

(K2 <- solve(K))
# solve(K, t(X2))

# https://en.wikipedia.org/wiki/Woodbury_matrix_identity#Derivation_from_LDU_decomposition
V <- drop(crossprod(x, U))
alpha <- 1 / drop(crossprod(x) - crossprod(V))
-alpha * V
K2[-1, 1]

alpha * tcrossprod(V) + diag(3)
K2[-1, -1]
