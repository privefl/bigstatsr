# simulating some data
N <- 73
M <- 430
X <- as.big.matrix(matrix(rnorm(N*M, sd = 5), N), type = "integer")
y <- sample(0:1, size = N, replace = TRUE)
covar <- matrix(rnorm(N * 3), N)

# error, only handle `double` `big.matrix` objects
tryCatch({
  test <- biglasso::biglasso(X, y, family = "binomial")
  print(summary(test$scale))
}, error = function(e) message("One error has been catched."))
# OK here
test2 <- big_spRegLog(X, y)
str(test2)

# how to use covariables?
X2 <- as.big.matrix(cbind(X[,], covar), type = "double")
test <- biglasso::biglasso(X2, y, family = "binomial", lambda.min = 0.01,
                           alpha = 0.5, penalty = "enet")
test2 <- big_spRegLog(X, y, covar.train = covar, alpha = 0.5)
# verification
print(all.equal(test2$lambda, test$lambda))
print(all.equal(test2$beta@x, test$beta[-1, ]@x))
print(all.equal(test2$intercept, test$beta[1, ]))
