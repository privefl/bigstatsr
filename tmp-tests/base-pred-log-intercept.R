
null_pred <- function(y, base) {

  ind0 <- which(y == 0)
  ind1 <- which(y == 1)
  x <- base

  f <- function(b0) {
    sum(1 / (1 + exp(b0 + base[ind1]))) -
      sum(1 / (1 + exp(-(b0 + base[ind0]))))
  }
  b0 <- stats::uniroot(f, c(-10, 10), check.conv = TRUE,
                       tol = .Machine$double.eps)$root

  c(b0, mean(1 / (1 + exp(-(b0 + x)))))
}

N <- 99000
x <- runif(N)
y <- sample(0:1, size = N, replace = TRUE)

null_pred(y, x)

x <- rep(0, N)
(y_bar <- mean(y))
log(y_bar / (1 - y_bar))
null_pred(y, x)
