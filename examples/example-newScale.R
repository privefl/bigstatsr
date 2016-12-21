
## new scaling function
# "y-aware" scaling center columns, then multiply them by betas of
# univariate linear regression. See https://goo.gl/8G8WMa for details.
big_scaleYaware <- function(y) {
  function(X, ind.train = seq(nrow(X))) {
    means <- big_colstats(X, ind.train)$sum / length(ind.train)
    betas <- big_univRegLin(X, y, ind.train)$estim
    list(mean = means, sd = 1 / betas)
  }
}
# Simulating some data
y <- rnorm(nrow(X), X[, 9], abs(X[, 9]))

X.svd <- big_SVD(X, fun.scaling = big_scaleYaware(y))
plot(X.svd$v[, 1], type = "h")
