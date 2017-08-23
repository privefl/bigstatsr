set.seed(1)

# simulating some data
N <- 73
M <- 430
X <- FBM(N, M, init = rnorm(N * M, sd = 5), type = "integer")
y <- sample(0:1, size = N, replace = TRUE)
covar <- matrix(rnorm(N * 3), N)

# error, only handle `double` `big.matrix` objects
X2 <- bigmemory::as.big.matrix(X[], type = "integer")
\dontrun{biglasso::biglasso(X2, y, family = "binomial")}

# OK here
test2 <- big_spLogReg(X, y)
str(test2)

# how to use covariables?
X2 <- bigmemory::as.big.matrix(cbind(X[,], covar), type = "double")
test <- biglasso::biglasso(X2, y, family = "binomial", lambda.min = 0.01,
                           alpha = 0.5, penalty = "enet")
test2 <- big_spLogReg(X, y, covar.train = covar, alpha = 0.5)
# verification
all.equal(test2$lambda, test$lambda)
all.equal(test2$beta@x, test$beta[-1, ]@x)
all.equal(test2$intercept, test$beta[1, ])
