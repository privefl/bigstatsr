# http://myweb.uiowa.edu/pbreheny/data/bcTCGA.html
bcTCGA <- readRDS("tmp-data/bcTCGA.rds")
table(bcTCGA$fData$chromosome)
X <- bcTCGA$X
dim(X)
X[1:5, 1:5]
y <- bcTCGA$y
hist(y)

ind.train <- sample(nrow(X), 400)
ind.test  <- setdiff(1:nrow(X), ind.train)

library(glmnet)
mod_all <- glmnet(X[ind.train, ], y[ind.train])
preds <- predict(mod_all, X[ind.test, ])
rmse <- colMeans(sweep(preds, 1, y[ind.test], '-')^2)

mod_cv <- cv.glmnet(X[ind.train, ], y[ind.train])
lambdas <- c(mod_cv$lambda.1se, mod_cv$lambda.min)

plot(mod_all$lambda, rmse, pch = 20,
     log = "x", ylim = c(0, max(rmse)))
points(mod_cv$lambda, mod_cv$cvm, pch = 20, col = 4)
abline(v = lambdas, col = 2:3)

preds_1se <- predict(mod_cv, X[ind.test, ], s = "lambda.1se")
preds_min <- predict(mod_cv, X[ind.test, ], s = "lambda.min")

(rmse_2 <- colMeans(sweep(preds_2, 1, y[ind.test], '-')^2))
abline(h = rmse_2, col = 2:3)

library(bigstatsr)
X2 <- as_FBM(X)
mod <- big_spLinReg(X2, y[ind.train], ind.train)
preds_CMSA <- predict(mod, X2, ind.test)
(rmse_CMSA <- mean((preds_CMSA - y[ind.test])^2))
abline(h = rmse_CMSA, col = 5)

cor(cbind(preds_1se, preds_min, preds_CMSA))

library(glmnetUtils)
mod_cva <- cva.glmnet(X[ind.train, ], y[ind.train])
plot(mod_cva)
