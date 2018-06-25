library(bigstatsr)
set.seed(1)

# simulating some data
N <- 500
M <- 730
X <- FBM(N, M, init = rnorm(N * M, sd = 5))
y <- 2 + rowSums(X[, 1:5]) + 3 * rnorm(N)
cor(rowSums(X[, 1:5]), y)
NCORES <- nb_cores()

ind.train <- sort(sample(nrow(X), 150))
ind.test <- setdiff(rows_along(X), ind.train)

test <- big_spLinReg(X, y[ind.train], ind.train = ind.train, alpha = 1)
# K = 10 predictions
str(preds <- predict(test, X, ind.row = ind.test))
# Combine them
preds2 <- rowMeans(preds)
cor(preds2, y[ind.test])
plot(preds2, y[ind.test], pch = 20); abline(0, 1, col = "red")

# Base prediction
y.base <- rowMeans(predict(test, X))
test2 <- big_spLinReg(X, y[ind.train], ind.train = ind.train, alpha = 0.01,
                      base.train = y.base[ind.train])
str(preds3 <- rowMeans(predict(test2, X, ind.row = ind.test)))
cor(preds3, y[ind.test])
plot(preds3 + y.base[ind.test], y[ind.test], pch = 20); abline(0, 1, col = "red")
cor(preds3 + y.base[ind.test], y[ind.test])

## TIMINGS
system.time(
  test <- big_spLinReg(X, y[ind.train], ind.train = ind.train,
                       ncores = 1, alpha = 1, return.all = TRUE)
)
tmp <- test[[1]][[1]]
plot(tmp$beta[1, ], pch = 20)
for (i in 2:5) points(tmp$beta[i, ], pch = 20, col = i)
plot(tmp$iter, pch = 20)
plot(tmp$loss.val, pch = 20)

system.time(
  test2 <- big_spLinReg(X, y[ind.train], ind.train = ind.train,
                       ncores = NCORES, alpha = 0.1, return.all = TRUE)
)
## 40-50-120 sec for alpha = 0.1

tmp2 <- test2[[1]][[1]]
plot(tmp2$iter, pch = 20)
plot(tmp2$loss.val, pch = 20)
plot(tmp2$beta[1, ], pch = 20)
for (i in 2:5) points(tmp2$beta[i, ], pch = 20, col = i)


test0 <- big_spLinReg(X, y[ind.train], ind.train = ind.train,
                     ncores = NCORES, alpha = 1)
pred.train <- rowMeans(predict(test0, X, ind.row = ind.train))
system.time(
  test3 <- big_spLinReg(X, y[ind.train], ind.train = ind.train,
                        base.train = pred.train,
                        ncores = NCORES, alpha = 0.01, return.all = TRUE)
)
tmp3 <- test3[[1]][[1]]
plot(tmp3$iter, pch = 20)
plot(tmp3$loss.val, pch = 20)
plot(tmp3$beta[1, ], pch = 20)
for (i in 2:5) points(tmp3$beta[i, ], pch = 20, col = i)


test3 <- big_spLinReg(X, y[ind.train], ind.train = ind.train,
                      base.train = pred.train,
                      ncores = NCORES, alpha = 0.01)
# K = 10 predictions
str(preds <- predict(test3, X, ind.row = ind.test))
# Combine them
preds2 <- rowMeans(preds)

cor(preds2, y[ind.test])  # 5%
plot(preds2, y[ind.test], pch = 20); abline(0, 1, col = "red")
