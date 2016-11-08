#' Date: 2016-10-26
#' Object: Compare results with prcomp
#' Results: OK

require(bigsnpr)
require(bigstatsr)
x <- AttachBigSNP("test_doc", "../bigsnpr/backingfiles")
X <- x$genotypes

f1 <- function(X) {
  means <- colmeans(X)
  p <- means / 2
  sds <- sqrt(2 * p * (1 - p))
  list(mean = means, sd = sds)
}

f2 <- function(X) {
  m <- ncol(X)
  list(mean = rep(0, m), sd = rep(1, m))
}

test <- big_randomSVD(X, fun.scaling = f1,
                      block.size = 100)
str(test)


# scaling
tmp <- f1(X)
X2 <- sweep(sweep(X[,], 2, tmp$mean, '-'), 2, tmp$sd, '/')

X2.svd <- svd(X2, nu = 10, nv = 10)
str(X2.svd)


plot(as.numeric(X2.svd$u), as.numeric(test$u), pch = 19)
plot(as.numeric(X2.svd$v), as.numeric(test$v), pch = 19)
