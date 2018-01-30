set.seed(1)

# simulating some data
N <- 230
M <- 730
X <- FBM(N, M, init = rnorm(N * M, sd = 5), type = "integer")
y01 <- as.numeric(X[, 1:5] + rnorm(N) > 0)
covar <- matrix(rnorm(N * 3), N)

ind.train <- sort(sample(nrow(X), 150))
ind.test <- setdiff(rows_along(X), ind.train)

test <- big_spLogReg(X, y01[ind.train], ind.train = ind.train,
                     covar.train = covar[ind.train, ],
                     warn = FALSE)
# K = 10 predictions
str(preds <- predict(test, X, ind.row = ind.test, covar.row = covar[ind.test, ]))
# Combine them
preds2 <- rowMeans(preds)

library(ggplot2)
qplot(preds2, fill = as.logical(y01[ind.test]), geom = "density", alpha = I(0.4)) +
  labs(fill = "Case?")
AUC(preds2, y01[ind.test])
