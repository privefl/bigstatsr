library(bigstatsr)
set.seed(1)

# simulating some data
N <- 230
M <- 730
X <- FBM(N, M, init = rnorm(N * M, sd = 5))
y <- rowSums(X[, 1:5]) + rnorm(N)
covar <- matrix(rnorm(N * 3), N)

ind.train <- sort(sample(nrow(X), 150))
ind.test <- setdiff(rows_along(X), ind.train)

test <- big_spLinReg(X, y[ind.train], ind.train = ind.train,
                     covar.train = covar[ind.train, ],
                     return.all = TRUE)[[1]]

str(test)
tmp <- test[[2]]
plot(tmp$loss, pch = 20); points(tmp$loss.val, pch = 20, col = 3)
x <- tmp$loss.val
x[-1] > 0.995 * x[-length(x)]
sapply(test, function(l) length(l$loss.val))

y2 <- as.numeric(y > 0)
test2 <- big_spLogReg(X, y2[ind.train], ind.train = ind.train,
                      covar.train = covar[ind.train, ],
                      return.all = TRUE, warn = TRUE)[[1]]
str(test2)
tmp2 <- test2[[8]]
plot(tmp2$loss, pch = 20); points(tmp2$loss.val, pch = 20, col = 3)
sapply(test2, function(l) length(l$loss.val))
x <- tmp2$loss.val
sum(x[-1] > 0.995 * x[-length(x)])
