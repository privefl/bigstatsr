X.desc <- big_attachExtdata()
n <- nrow(X.desc)
y <- rnorm(n)
covar <- matrix(rnorm(n * 3), n)

X1 <- attach.BM(X.desc)[, 1] # only first column of the `big.matrix`

# Without covar
test <- big_univLinReg(X.desc, y)
## new class `mhtest`
class(test)
attr(test, "transfo")
attr(test, "predict")
## plot results
plot(test)
plot(test, type = "Volcano")
## To get p-values associated with the test
test$p.value <- predict(test)
str(test)
summary(lm(y ~ X1))$coefficients[2, ]

# With all data
str(big_univLinReg(X.desc, y, covar = covar))
summary(lm(y ~ X1 + covar))$coefficients[2, ]

# With only half of the data
ind.train <- sort(sample(n, n/2))
str(big_univLinReg(X.desc, y[ind.train],
                   covar.train = covar[ind.train, ],
                   ind.train = ind.train))
summary(lm(y ~ X1 + covar, subset = ind.train))$coefficients[2, ]
