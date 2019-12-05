mat <- matrix(rnorm(200), 20)
svd <- svd(mat)
pca <- prcomp(mat, center = FALSE)
all.equal(
  pca$sdev^2 / sum(pca$sdev^2),
  svd$d^2 / sum(mat^2))

pca2 <- prcomp(mat, scale. = TRUE)
mat2 <- scale(mat)
svd2 <- svd(mat2)
all.equal(
  pca2$sdev^2 / sum(pca2$sdev^2),
  svd2$d^2 / sum(mat2^2))

library(bigstatsr)

big_norm <- function(X, ind.row = rows_along(X), ind.col = cols_along(X),
                     center = rep(0, length(ind.col)),
                     scale = rep(1, length(ind.col))) {
  bigassertr::assert_lengths(center, scale, ind.col)
  stats <- big_colstats(X, ind.row, ind.col)
  n <- length(ind.row)
  sum(((n - 1) * stats$var + n * (stats$sum / n - center)^2) / scale^2)
}

X <- big_attachExtdata()
ind.row <- sample(nrow(X), 100)
ind.col <- sample(ncol(X), 200)
center <- rnorm(200)
scale <- runif(200, 0.5)

n1 <- norm(scale(X[ind.row, ind.col], center = center, scale = scale), "F")

n2 <- big_norm(X, ind.row, ind.col, center, scale)
n1 / sqrt(n2)

obj.svd <- big_randomSVD(X, big_scale(), k = 20)
var_exp <- obj.svd$d^2 / big_norm(X, center = obj.svd$center, scale = obj.svd$scale)
signif(var_exp, 2)
round(cumsum(var_exp), 3)
