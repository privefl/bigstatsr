
library(bigstatsr)


N <- 2000
M <- 500
a <- matrix(0, N, M)
a[] <- rnorm(length(a))
b <- as.big.matrix(a, type = "char", shared = FALSE)

require(microbenchmark)

print(microbenchmark(
  r1 <- big_crossprodSelf(b, big_scale(), block.size = 50),
  r2 <- big_crossprodSelf2(b, big_scale(), block.size = 50),
  times = 2
))

r1 <- big_crossprodSelf(b, big_scale(), block.size = 50)

r1$K[1:5, 1:5]
all.equal(r1$K, as.matrix(r2$K))


require(Matrix)

x <- Matrix(NA_real_, 50, 50)
x[1:50, 1:50] <- tcrossprod(1:50)
