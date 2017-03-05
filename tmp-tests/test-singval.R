a <- matrix(0, 500, 3000)
a[] <- rnorm(length(a), sd = 5)

n <- c(10, 30, 100, 300, 500)
m <- c(10, 30, 100, 300, 1000, 3000)

res <- expand.grid(n, m, 0)
c <- 1
for (j in seq_along(m)) {
  for (i in seq_along(n)) {
    res[c, 3] <- svd(scale(a[1:n[i], 1:m[j]]), nu = 0, nv = 0)$d[1]
    c <- c + 1
  }
}

res[, 4] <- sqrt(res[, 1])
res[, 5] <- sqrt(res[, 2])

mylm <- lm(Var3 ~ V4 + V5, data = res)
summary(mylm)

