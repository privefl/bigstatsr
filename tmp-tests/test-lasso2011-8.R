require(bigsnpr)
require(bigstatsr)
require(biglasso)

backingfile <- "../thesis-celiac/backingfiles/celiac_sub2_impute1.bk"
# backingfile <- "../bigsnpr/backingfiles/celiac_impute1_sub1.bk"
celiac <- AttachBigSNP(backingfile)
X <- celiac$genotypes
y <- celiac$fam$affection - 1

# print(system.time(
#   maf <- snp_MAF(X)
# ))
#
# ind.keep <- snp_clumping(celiac, S = maf, thr.corr = 0.05,
#                          exclude = (maf < 0.05), ncores = 12)
#
# X.svd <- big_randomSVD(X, fun.scaling = snp_scaleBinom, ncores = 4,
#                        ind.col = ind.keep, verbose = TRUE)
# saveRDS(X.svd, "tmp-tests/X.svd.rds")
X.svd <- readRDS("tmp-tests/X.svd.rds")

print(system.time(
  test2 <- biglasso(X, y, covar = NULL, # X.svd$u,
                    family = "binomial", screen = "COPY-SSR", verbose = TRUE,
                    ncores = 12)
))
# 1510 sec
# 187 sec with 10 cores
# 172 sec with 12 cores
print(ncol(X))
print(dim(test2$beta))

print(system.time(
  test3 <- biglasso(X, y, covar = X.svd$u,
                    family = "binomial", screen = "COPY-SSR", verbose = TRUE,
                    ncores = 10)
))
print(dim(test3$beta))

predict <- function(X, mod) {
  m <- ncol(X)
  pred <- multScaled(X, as.matrix(mod$beta[-1, ]), ind.train = seq(nrow(X)),
                     block.size = 1000, vec.center = rep(0, m),
                     vec.scale = rep(1, m), use.Eigen = TRUE)
  pred2 <- sweep(pred, 2, mod$beta[1, ], '+')
  apply(pred2, 2, snp_aucSample, target = y, nsim = 1e5)
}
testAUC <- predict(X, test2)
plot(testAUC)
nbBeta <- colSums(test2$beta[-1, ] != 0)
plot(nbBeta, testAUC)

print(system.time(
  test4 <- biglasso(X, y, covar = NULL, penalty = "enet", alpha = 0.1,
                    family = "binomial", screen = "COPY-SSR", verbose = TRUE,
                    ncores = 12, lambda.min = 1e-3, dfmax = 1000)
))
testAUC2 <- predict(X, test4)
nbBeta2 <- colSums(test4$beta[-1, ] != 0)
plot(nbBeta2[-1], testAUC2[-1], xlim = c(0, 200))
plot(nbBeta2, test4$loss, xlim = c(0, 500)) # better
pred <- produ(X, test4$beta[-1, 35])
snp_aucSample(pred, y)
