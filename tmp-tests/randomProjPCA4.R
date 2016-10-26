#' Date: 2016-10-26
#' Object: Compare results with prcomp
#' Results: OK

require(bigsnpr)
require(bigstatr)
x <- AttachBigSNP("test_doc", "../bigsnpr/backingfiles")
X <- x$genotypes

test <- RandomProjPCA(X, block.size = 1e3, use.Eigen = TRUE)
str(test)
approx <- sweep(test$u, 2, test$d, '*')

# scaling
p <- colmeans(X) / 2
sd <- sqrt(2 * p * (1 - p))
X2 <- sweep(sweep(X[,], 2, 2 * p, '-'), 2, sd, '/')

pca <- prcomp(X2, center = FALSE)
true <- pca$x[, 1:10]


plot(as.numeric(true), as.numeric(approx), pch = 19)

test <- RandomProjPCA(X, block.size = 1e3, use.Eigen = FALSE)
str(test)
approx2 <- sweep(test$u, 2, test$d, '*')
plot(as.numeric(true), as.numeric(approx2), pch = 19)
