X <- FBM(13, 17, init = rnorm(221))

# Comparing with cor
K <- big_cor(X)
dim(K)

true <- cor(X[])
all.equal(K, true)

# Using only half of the data
n <- nrow(X)
ind <- sort(sample(n, n/2))
K2 <- big_cor(X, ind.row = ind)
dim(K2)

true2 <- cor(X[ind, ])
all.equal(K2, true2)
