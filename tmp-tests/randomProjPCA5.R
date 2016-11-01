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

test <- ParallelRandomProjPCA(X, fun.scaling = f2,
                              block.size = 100, ncores = 2,
                              backingpath = "../bigsnpr/backingfiles")
str(test)
approx <- sweep(test$u, 2, test$d, '*')

# scaling
p <- colmeans(X) / 2
sd <- sqrt(2 * p * (1 - p))
X2 <- sweep(sweep(X[,], 2, 2 * p, '-'), 2, sd, '/')

pca <- prcomp(X2, center = FALSE)
true <- pca$x[, 1:10]


plot(as.numeric(true), as.numeric(approx), pch = 19)
