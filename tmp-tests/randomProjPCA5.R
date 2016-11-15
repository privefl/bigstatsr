#' Date: 2016-10-26
#' Object: Compare results with prcomp
#' Results: OK

require(bigsnpr)
require(bigstatsr)
x <- AttachBigSNP("test_doc", "../bigsnpr/backingfiles")
X <- x$genotypes

f1 <- function(X, ind.train) {
  means <- big_colstats(X, ind.train)$sum / length(ind.train)
  p <- means / 2
  sds <- sqrt(2 * p * (1 - p))
  list(mean = means, sd = sds)
}

f2 <- function(X, ind.train) {
  m <- ncol(X)
  list(mean = rep(0, m), sd = rep(1, m))
}

test <- big_randomSVD(X, fun.scaling = f1, block.size = 100,
                      backingpath = x$backingpath, ncores = 2)
str(test)


X2 <- big_transpose(X, shared = TRUE)

test2 <- big_randomSVD(X2, fun.scaling = f1, block.size = 100, ncores = 2)
str(test2)

