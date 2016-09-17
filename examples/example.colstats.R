# Simulating some data
X <- big.matrix(10, 10)
X[] <- rnorm(100)

# Check the results
print(test <- colsums(X))
print(test - colSums(X[,]))

# Only with the first 5 rows
ind <- 1:5
print(test <- colsums(X, ind))
print(test - colSums(X[ind, ]))

# NA in, NA out
X[2, 5] <- NA
print(colsums(X))
