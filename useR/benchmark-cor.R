a <- matrix(0, 1e5, 5e3); a[] <- rnorm(length(a))
print(system.time(
  corr1 <- cor(a)
))

library(bigstatsr)
A <- as.big.matrix(a)

print(system.time(
  corr2 <- big_cor(A)
))

all.equal(a, A[])
