# simulating some data
N <- 73
M <- 430
X <- FBM(N, M, init = rnorm(N * M, sd = 5))
y <- sample(0:1, size = N, replace = TRUE)
covar <- matrix(rnorm(N * 3), N)

ind.train <- sort(sample(N, N/2))
mod <- big_spLogReg(X, y[ind.train], ind.train = ind.train,
                    covar.train = covar[ind.train, ])
ind.test <- setdiff(rows_along(X), ind.train)
pred <- predict(mod, X = X, ind.row = ind.test,
                covar.row = covar[ind.test, ])
