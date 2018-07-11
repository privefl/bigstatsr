library(bigstatsr)

# simulating some data
N <- 2300
M <- 7300
X <- FBM(N, M, init = rnorm(N * M, sd = 5))
y <- rowSums(X[, 1:5]) + rnorm(N)
covar <- matrix(rnorm(N * 3), N)

ind.train <- sort(sample(nrow(X), 1500))
ind.test <- setdiff(rows_along(X), ind.train)

system.time(
  test <- big_spLinReg(X, y[ind.train], ind.train = ind.train,
                       covar.train = covar[ind.train, ],
                       alpha = 0.001, return.all = TRUE)
) # 0.2sec -> 1.2

tmp <- test[[1]][[1]]
class(tmp$beta)
