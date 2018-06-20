N <- 100
x <- runif(N)
y <- sample(0:1, size = N, replace = TRUE)

ind0 <- which(y == 0)
ind1 <- which(y == 1)

f <- function(b0) {
  sum(1 / (1 + exp(b0 + x[ind1]))) - sum(1 / (1 + exp(-(b0 + x[ind0]))))
}
str(xmin <- uniroot(f, c(-20, 20)), tol = sqrt(.Machine$double.eps))

x <- rep(0, N)
str(xmin <- uniroot(f, c(-10, 10)), tol = sqrt(.Machine$double.eps))
log(mean(y) / (1 - mean(y)))


curve(Vectorize(f)(x), from = -2, to = 2)
