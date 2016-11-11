# Simulating some data
X <- big.matrix(41, 17)
X[] <- rnorm(length(X))

# No scaling
big_noscale <- big_scale(center = FALSE, scale = FALSE)
print(class(big_noscale)) # big_scale returns a new function
print(big_noscale(X))
big_noscale2 <- big_scale(center = FALSE)
print(big_noscale2(X)) # you can't scale without centering

# Centering
big_center <- big_scale(scale = FALSE)
print(big_center(X))
# + scaling
print(test <- big_scale()(X, 1:5)) # note that big_scale() is a function
print(all.equal(test$mean, colMeans(X[1:5, ])))
print(all.equal(test$sd, apply(X[1:5, ], 2, sd)))
