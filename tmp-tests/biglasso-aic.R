set.seed(1)

# Simulating some data
N <- 530
M <- 730
x <- matrix(rnorm(N * M, sd = 5), N)
y <- rowSums(x[, 1:5]) + 8 * rnorm(N)

library(biglasso)
X <- as.big.matrix(x, shared = FALSE)

# Training
ind.train <- sample(N, 400)
mod <- biglasso(X, y, row.idx = ind.train)

lambda <- mod$lambda
n <- mod$n
loss.train <- mod$loss / n

# over-fitting on training, use AIC instead
plot(lambda, loss.train, pch = 20)
k <- 1 + ncol(X) - mod$rejections
aic <- 2 * (k + loss.train)
bic <- k * log(n) + 2 * loss.train
aic2 <- n * (log(2 * pi) + 1 + log(loss.train)) + 2 * k
plot(lambda, aic, pch = 20)
plot(lambda, bic, pch = 20)
plot(lambda, aic2, pch = 20)
which.min(aic)
k[which.min(aic)]
mod$beta[, which.min(aic)]

# Check on test set
ind.test <- setdiff(seq_len(N), ind.train)
preds <- predict(mod, X, row.idx = ind.test)
y.test <- y[ind.test]
loss.test <- apply(preds, 2, function(pred) {
  mean((pred - y.test)^2)
})
plot(lambda, loss.train, pch = 20, ylim = c(0, max(aic)))
points(lambda, aic, pch = 20, col = "blue")
points(lambda, bic, pch = 20, col = "green")
points(lambda, loss.test, pch = 20, col = "red")

library(glmnet)
fit <- glmnet(x[ind.train, ], y[ind.train], family = "gaussian",
              alpha = 1, lambda.min = 0.05)
fit$df
all.equal(fit$df + 1, k)
# k
#
# tLL <- fit$nulldev - deviance(fit)
#
# k <- fit$df + 1
# n <- fit$nobs
# AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
# plot(lambda, AICc, pch = 20)
# which.min(AICc)
# k[which.min(AICc)]
#
# BIC<-log(n)*k - tLL
# BIC
# plot(BIC)
# which.min(BIC)

fit2 <- ncvreg(x[ind.train, ], y[ind.train], family = "gaussian",
               penalty = "lasso", alpha = 1)
k2 <- colSums(fit2$beta != 0)
plot(AIC(fit2))
all.equal(mod$lambda, fit2$lambda)
fit2
plot(fit2)
aic3 <- AIC(fit2)
plot(aic3)
k <- 1 + ncol(X) - mod$rejections
n <- mod$n
aic4 <- n * (log(2 * pi) + 1 + log(fit2$loss)) + 2 * (k2 + 1)
(aic4 - aic3) / n
plot((aic4 - aic3))

aic5 <- n * (log(2 * pi) + 1 + log(loss.train)) + 2*k+2*k*(k+1)/(n-k-1)
aic5 - aic3
plot(aic5)
k[which.min(aic5)]
