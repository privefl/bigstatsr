# http://myweb.uiowa.edu/pbreheny/data/bcTCGA.html
bcTCGA <- readRDS("tmp-data/bcTCGA.rds")
table(bcTCGA$fData$chromosome)
X <- bcTCGA$X
dim(X)
X[1:5, 1:5]
y <- bcTCGA$y
hist(y)

timing <- function(expr) system.time(expr)[3]

RMSE <- function(pred, y.test) {
  stopifnot(length(pred) == length(y.test))
  mean((pred - y.test)^2)
}

set.seed(1)
res_all <- replicate(200, simplify = FALSE, {

  ind.train <- sample(nrow(X), 400)
  ind.test  <- setdiff(1:nrow(X), ind.train)

  library(glmnet)
  t_all <- timing(mod_all <- glmnet(X[ind.train, ], y[ind.train]))
  preds_all <- predict(mod_all, X[ind.test, ])
  rmse_all <- apply(preds_all, 2, RMSE, y[ind.test])

  t_cv <- timing(mod_cv <- cv.glmnet(X[ind.train, ], y[ind.train]))
  preds_1se <- predict(mod_cv, X[ind.test, ], s = "lambda.1se")
  rmse_1se <- RMSE(preds_1se, y[ind.test])
  preds_min <- predict(mod_cv, X[ind.test, ], s = "lambda.min")
  rmse_min <- RMSE(preds_min, y[ind.test])

  library(bigstatsr)
  t_CMSA <- timing({
    X2 <- as_FBM(X)
    mod_CMSA <- big_spLinReg(X2, y[ind.train], ind.train)
  })
  preds_CMSA <- predict(mod_CMSA, X2, ind.test)
  rmse_CMSA <- RMSE(preds_CMSA, y[ind.test])

  library(glmnetUtils)
  ALPHA <- c(1, 0.5, 0.1)
  t_cva <- timing(mod_cva <- cva.glmnet(X[ind.train, ], y[ind.train], alpha = ALPHA))
  alpha <- ALPHA[which.min(sapply(mod_cva$modlist, function(mod) min(mod$cvm)))]
  rmse_cva <- RMSE(predict(mod_cva, X[ind.test, ], alpha = alpha), y[ind.test])

  t_CMSA2 <- timing({
    X2 <- as_FBM(X)
    mod_CMSA2 <- big_spLinReg(X2, y[ind.train], ind.train, alphas = ALPHA)
  })
  preds_CMSA2 <- predict(mod_CMSA2, X2, ind.test)
  rmse_CMSA2 <- RMSE(preds_CMSA2, y[ind.test])

  tibble::tribble(
    ~method,        ~timing,  ~rmse,
    "glmnet_best",  t_all,    min(rmse_all),
    "glmnet_min",   t_cv,     rmse_1se,
    "glmnet_1se",   t_cv,     rmse_min,
    "CMSA",         t_CMSA,   rmse_CMSA,
    "glmnet_cva",   t_cva,    rmse_cva,
    "CMSA2",        t_CMSA2,  rmse_CMSA2
  )

})

res <- do.call(rbind, res_all)
res$run_number <- rep(seq_along(res_all), each = 6)
res
