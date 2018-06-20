library(bigstatsr)
set.seed(2)

# simulating some data
N <- 530
M <- 730
X <- FBM(N, M, init = rnorm(N * M, sd = 5))
y01 <- as.numeric((rowSums(X[, 1:5]) + 2 * rnorm(N)) > 0)
AUC(rowSums(X[, 1:5]), y01)
NCORES <- nb_cores()

ind.train <- sort(sample(nrow(X), 300))
ind.test <- setdiff(rows_along(X), ind.train)

system.time(
  test <- big_spLogReg(X, y01[ind.train], ind.train = ind.train,
                       ncores = NCORES, alpha = 1, return.all = TRUE)
)
tmp <- test[[1]][[1]]
plot(tmp$iter, pch = 20)
plot(tmp$loss.val, pch = 20)
plot(tmp$beta[1, ], pch = 20)
for (i in 2:5) points(tmp$beta[i, ], pch = 20, col = i)



system.time(
  test2 <- big_spLogReg(X, y01[ind.train], ind.train = ind.train,
                       ncores = NCORES, alpha = 0.01, return.all = TRUE)
)
# alpha = 0.1  -> 10 sec
# alpha = 0.01 -> 72 sec / 74 / 60 / 44 / 64
tmp2 <- test2[[1]][[1]]
plot(tmp2$iter, pch = 20)
plot(tmp2$loss.val, pch = 20)
plot(tmp2$beta[1, ], pch = 20)
for (i in 2:5) points(tmp2$beta[i, ], pch = 20, col = i)



test <- big_spLogReg(X, y01[ind.train], ind.train = ind.train,
                     ncores = NCORES, alpha = 0.1)
# K = 10 predictions
str(preds <- predict(test, X, ind.row = ind.test))
# Combine them
preds2 <- rowMeans(preds)

library(ggplot2)
qplot(preds2, fill = as.logical(y01[ind.test]), geom = "density", alpha = I(0.4)) +
  labs(fill = "Case?")
AUC(preds2, y01[ind.test])
