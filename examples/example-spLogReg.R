set.seed(2)

# simulating some data
N <- 230
M <- 730
X <- FBM(N, M, init = rnorm(N * M, sd = 5))
y01 <- as.numeric((rowSums(X[, 1:5]) + 2 * rnorm(N)) > 0)
covar <- matrix(rnorm(N * 3), N)

ind.train <- sort(sample(nrow(X), 150))
ind.test <- setdiff(rows_along(X), ind.train)

test <- big_spLogReg(X, y01[ind.train], ind.train = ind.train,
                     covar.train = covar[ind.train, ])

preds <- predict(test, X, ind.row = ind.test, covar.row = covar[ind.test, ])
library(ggplot2)
qplot(preds, fill = as.logical(y01[ind.test]), geom = "density", alpha = I(0.4)) +
  labs(fill = "Case?") +
  theme_bigstatsr()
AUC(preds, y01[ind.test])
