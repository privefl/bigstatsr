library(bigsnpr)

celiac <- snp_attach("../paper1-packages/backingfiles/celiacQC.rds")

G <- celiac$genotypes
y01 <- celiac$fam$affection - 1
NCORES <- nb_cores()


# covar <- matrix(rnorm(length(ind.train) * 10), ncol = 10)
CHR <- celiac$map$chromosome
POS <- celiac$map$physical.pos
ind.keep <- snp_pruning(G, CHR, exclude = snp_indLRLDR(CHR, POS), thr.r2 = 0.1, ncores = NCORES)
obj.svd <- big_randomSVD(G, snp_scaleBinom(), ind.col = ind.keep, k = 10, ncores = NCORES)

ind.train <- sort(sample(nrow(G), 12e3))

# debug(bigstatsr:::COPY_biglasso_main)
system.time(
  test <- big_spLogReg(
    G, y01[ind.train], ind.train,
    covar.train = obj.svd$u[ind.train, ],
    ncores = NCORES
  )
) # 116 / 45

ind.test <- setdiff(rows_along(G), ind.train)
preds <- predict(test, X = G, ind.row = ind.test,
                 covar.row = obj.svd$u[ind.test, ])
pred0 <- rowMeans(preds)

library(ggplot2)
qplot(pred0, fill = as.factor(y01[ind.test]), geom = "density", alpha = I(0.4))
AUC(pred0, target = y01[ind.test])
pAUC(pred0, target = y01[ind.test])


# system.time(
#   gwas <- big_univLogReg(G, y01[ind.train], ind.train,
#                          covar.train = obj.svd$u[ind.train, ],
#                          ncores = NCORES)
# ) # 444 / 81


plot(test[[1]]$loss, pch = 20)
points(test[[1]]$loss.val, pch = 20, col = "red")

tmp <- lapply(test, function(x) {
  best <- which.min(x$loss.val)
  ind <- seq_along(x$ind.col)
  structure(list(
    intercept  = unname(x$intercept[best]),
    beta.X     = x$beta[ind, best],
    beta.covar = x$beta[-ind, best],
    ind.col    = x$ind.col
  ), class = "big_sp_best")
})



sapply(tmp, function(x) sum(x$beta.X != 0))
sapply(tmp, function(x) sum(x$beta.covar != 0))

tmp2 <- predict(test[[1]], X = G, ind.row = ind.test, covar.row = obj.svd$u[ind.test, ])

tmp2 <- sapply(tmp, function(obj) {
  predict(obj, X = G, ind.row = ind.test, covar.row = obj.svd$u[ind.test, ], proba = TRUE)
})
plot(tmp2)
hist(apply(tmp2, 1, sd))
AUC(pred1 <- rowMeans(tmp2), y01[ind.test])
AUC(pred2 <- apply(tmp2, 1, mean, trim = 0.1), y01[ind.test])
AUC(pred3 <- apply(tmp2, 1, min), y01[ind.test])

pAUC <- function(pred, target, p = 0.1) {
  val.min <- min(target)
  q <- quantile(pred[target == val.min], probs = 1 - p)
  ind <- (target != val.min) | (pred > q)
  bigstatsr::AUC(pred[ind], target[ind]) * p
}
pAUC(pred1, y01[ind.test])
pAUC(pred2, y01[ind.test])
pAUC(pred3, y01[ind.test])

library(ggplot2)
qplot(pred1, fill = as.factor(y01[ind.test]), geom = "density", alpha = I(0.4))
qplot(pred2, fill = as.factor(y01[ind.test]), geom = "density", alpha = I(0.4))
plot(pred1, pred2)
