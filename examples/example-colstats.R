# Simulating some data
X <- big.matrix(41, 7)
X[] <- rnorm(length(X))

# Check the results
print(test <- big_colstats(X))

# Only with the first 10 rows
ind <- 1:10
print(test <- big_colstats(X, ind))
print(all.equal(test$sum, colSums(X[ind, ])))
print(all.equal(test$var, apply(X[ind, ], 2, var)))

# deduce mean and sd
# note that the are also implemented in big_scale()
means <- test$sum / length(ind) # if using all rows,
                                # divide by nrow(X) instead
print(all.equal(means, colMeans(X[ind, ])))
sds <- sqrt(test$var)
print(all.equal(sds, apply(X[ind, ], 2, sd)))

# NA in, NA out
X[2, 5] <- NA
print(big_colstats(X))
