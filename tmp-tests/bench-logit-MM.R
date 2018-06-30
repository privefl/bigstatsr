Rcpp::sourceCpp('tmp-save/test-logit-MM.cpp')

library(bigsnpr)
celiac <- snp_attach("../paper-packages/backingfiles/celiacQC.rds")
G <- celiac$genotypes
y01 <- celiac$fam$affection - 1
obj.svd <- readRDS("../paper-packages/backingfiles/celiacSVD.rds")

# ind.col <- seq_len(2000)
ind.col <- which(celiac$map$chromosome == 6)
system.time(
  test <- big_univLogReg(G, y01, ind.col = ind.col,
                         covar.train = obj.svd$u)
)

U <- svd(cbind(1, obj.svd$u))$u
mod0 <- stats::glm(y01 ~ U - 1, family = "binomial")
p0 <- mod0$fitted
w0 <- p0 * (1 - p0)
z0 <- log(p0 / (1 - p0)) * w0 + (y01 - p0)

X <- G[, ind.col]
# system.time(
#   test2 <- IRLS(X, U = U, y = y01, z0 = z0)
# )
# stopifnot(all.equal(test2$estim, test$estim))

Rcpp::sourceCpp('tmp-save/test-logit-eigen.cpp')
system.time(
  test3 <- IRLS2(X, covar = cbind(0, 1, obj.svd$u), y = y01, z0 = z0, w0 = w0)
)
str(test3$estim)
stopifnot(all.equal(test3$estim, test$estim))
str(test3$std.err)
stopifnot(all.equal(test3$std.err, test$std.err))
table(test3$niter, test$niter)
