# Simulating some data
data("trees")
N <- nrow(trees)
covar <- matrix(rnorm(N * 3), N)
X <- as.big.matrix(as.matrix(trees[, -1]))
y <- trees[, 1]

# without covar
lmVol0 <- summary(lm(Girth ~ Volume, data = trees))
lmHei0 <- summary(lm(Girth ~ Height, data = trees))

print(big_univRegLin(X, y))
print(rbind(lmHei0$coefficients[2, ], lmVol0$coefficients[2, ]))

# With all data
# lm
lmVol <- summary(lm(Girth ~ Volume + covar, data = trees))
lmHei <- summary(lm(Girth ~ Height + covar, data = trees))

print(big_univRegLin(X, y, covar = covar))
print(rbind(lmHei$coefficients[2, ], lmVol$coefficients[2, ]))

# With only half of the data
ind.train <- sort(sample(N, N / 2))

# lm
lmVol2 <- summary(lm(Girth ~ Volume + covar, data = trees, subset = ind.train))
lmHei2 <- summary(lm(Girth ~ Height + covar, data = trees, subset = ind.train))

print(big_univRegLin(X, y,
                     covar = covar[ind.train, ],
                     ind.train = ind.train))
print(rbind(lmHei2$coefficients[2, ], lmVol2$coefficients[2, ]))
