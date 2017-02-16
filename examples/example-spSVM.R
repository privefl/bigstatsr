# simulating some data
N <- 73
M <- 430
X <- as.big.matrix(matrix(rnorm(N*M, sd = 5), N))
y <- sample(0:1, size = N, replace = TRUE)
y.factor <- factor(y, levels = c(1, 0))
covar <- matrix(rnorm(N * 3), N)

# error, only handle standard R matrices
tryCatch(test <- sparseSVM::sparseSVM(X, y),
         error = function(e) message("One error has been catched."))
# OK here
test2 <- big_spSVM(X, y)
str(test2)

# how to use covariables?
X2 <- cbind(X[,], covar)
test <- sparseSVM::sparseSVM(X2, y.factor, alpha = 0.5)
test2 <- big_spSVM(X, y, covar.train = covar, alpha = 0.5)
# verification
print(all.equal(test2$lambda, test$lambda))
print(all.equal(test2$beta, test$weights[-1, ], check.attributes = FALSE))
print(all.equal(test2$intercept, test$weights[1, ]))
