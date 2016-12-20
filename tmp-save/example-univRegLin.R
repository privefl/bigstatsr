# Simulating some data
N <- 1000
x <- rnorm(N)
x.big <- as.big.matrix(cbind(x, x+1, 2*x))

# With all data
y2 <- x + rnorm(length(x), 0, 0.5)
res <- matrix(0, 3, ncol(x.big))
for (j in 1:ncol(x.big)) {
  mylm <- lm(y2 ~ x.big[, j])
  res[1:2, j] <- mylm$coefficients
  res[3, j] <- summary(mylm)$r.squared
}
print(res)
print(big_univRegLin(x.big, y2))

# With only half of the data
ind.train <- sort(sample(length(x), length(x) / 2))
res <- matrix(0, 3, ncol(x.big))
for (j in 1:ncol(x.big)) {
  mylm <- lm(y2[ind.train] ~ x.big[ind.train, j])
  res[1:2, j] <- mylm$coefficients
  res[3, j] <- summary(mylm)$r.squared
}
print(res)
print(big_univRegLin(x.big, y2, ind.train))
