### Data simulation
set.seed(1)
N <- 500
M <- 2000
MC <- 100
X <- matrix(rnorm(N * M), N)
set <- sample(M, MC)
eff <- rnorm(MC - 1)
# Many small effects and one large effect
y <- X[, set[-1]] %*% eff + X[, set[1]] * 10
# Add some noise
y <- y + rnorm(N, sd = sd(y) / 20)

### Prediction with lasso on all variables
library(glmnet)
ind.train <- sample(N, 0.8 * N)
ind.test <- setdiff(1:N, ind.train)
CV <- cv.glmnet(X[ind.train, ], y[ind.train])
mod.lasso <- glmnet(X[ind.train, ], y[ind.train], lambda = CV$lambda.1se)
pred.lasso <- predict(mod.lasso, X[ind.test, ])

plot(pred.lasso, y[ind.test], pch = 20); abline(0, 1, col = "red")
sqrt(mean((pred.lasso - y[ind.test])^2)) # RMSE: 3.2

### Prediction with linear model using only the variable with strong effect
mod.lm <- lm(y ~ X, data = data.frame(y = y[ind.train], X = X[ind.train, set[1]]))
summary(mod.lm)
pred.lm <- predict(mod.lm, data.frame(y = y, X = X[, set[1]]))

plot(pred.lm[ind.test], y[ind.test], pch = 20); abline(0, 1, col = "red")
sqrt(mean((pred.lm[ind.test] - y[ind.test])^2)) # RMSE: 9.3

### Prediction with lasso on residuals of linear model
CV.resid <- cv.glmnet(X[ind.train, ], y[ind.train] - pred.lm[ind.train])
mod.lasso.resid <- glmnet(X[ind.train, ], y[ind.train] - pred.lm[ind.train],
                          lambda = CV.resid$lambda.1se)
pred.lasso2 <- pred.lm[ind.test] + predict(mod.lasso.resid, X[ind.test, ])

plot(pred.lasso2, y[ind.test], pch = 20); abline(0, 1, col = "red")
sqrt(mean((pred.lasso2 - y[ind.test])^2)) # RMSE: 2.6


### Prediction with lasso while not penalizing this variable:
pf <- rep(1, ncol(X)); pf[set[1]] <- 0
CV2 <- cv.glmnet(X[ind.train, ], y[ind.train], penalty.factor = pf)
mod.lasso2 <- glmnet(X[ind.train, ], y[ind.train], lambda = CV2$lambda.1se,
                     penalty.factor = pf)
pred.lasso2 <- predict(mod.lasso2, X[ind.test, ])

plot(pred.lasso2, y[ind.test], pch = 20); abline(0, 1, col = "red")
sqrt(mean((pred.lasso2 - y[ind.test])^2)) # RMSE: 2.4

library(bigstatsr)
X2 <- as_FBM(X)
mod.bigstatsr <- big_spLinReg(X2, y[ind.train], ind.train = ind.train,
                              pf.X = pf, power_scale = c(0, 0.5, 1),
                              power_adaptive = c(0, 0.5, 1, 1.5, 2),
                              lambda.min.ratio = 1e-5, ncores = 4)
str(mod.bigstatsr, max.level = 2)
summary(mod.bigstatsr, sort = TRUE)
plot(mod.bigstatsr) +
  ggplot2::facet_grid(power_adaptive ~ power_scale, labeller = signif)
beta <- summary(mod.bigstatsr, best.only = TRUE)$beta[[1]]

cols <- rep(1, ncol(X)); cols[set] <- 2; cols[set[1]] <- 3
plot(beta, pch = 20, col = cols)


pred.lasso3 <- predict(mod.bigstatsr, X2, ind.row = ind.test)

plot(pred.lasso3, y[ind.test], pch = 20); abline(0, 1, col = "red")
sqrt(mean((pred.lasso3 - y[ind.test])^2)) # RMSE: 3.2

tmp <- bigstatsr:::summaries(X2, y_diff.train = y[ind.train] - mean(y[ind.train]),
                      ind.train = ind.train, ind.col = cols_along(X2),
                      ind.sets = rep_len(1:2, length(ind.train)),
                      covar.train = matrix(0, length(ind.train), 0), K = 2)
tmp$center[2, ]
plot(t(tmp$resid), pch = 20, col = cols)
plot(colMeans(tmp$resid), pch = 20, col = cols)
hist(S <- colMeans(tmp$resid))
abline(v = c(-1, 1) * bigutilsr::tukey_mc_up(S, alpha = 1e-5), col = "red")

hist(summary(mod.bigstatsr)$beta[[1]], breaks = 50)
