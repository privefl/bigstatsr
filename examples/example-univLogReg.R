X.desc <- big_attachExtdata()
n <- nrow(X.desc)
y <- sample(0:1, size = n, replace = TRUE)
covar <- matrix(rnorm(n * 3), n)

# Without covar
str(big_univLogReg(X.desc, y))

# With all data
str(big_univLogReg(X.desc, y, covar.train = covar))

# With only half of the data
ind.train <- sort(sample(n, n/2))
str(big_univLogReg(X.desc, y[ind.train],
                   covar.train = covar[ind.train, ],
                   ind.train = ind.train))
