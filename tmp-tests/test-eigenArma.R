library(bigstatsr)
a <- big.matrix(1e4, 1.5e4)
a[] <- rnorm(length(a))
a <- tcrossprod(a[,])
a <- as.big.matrix(a)
a <- a[,]
gc()

print(system.time(
  gc(test <- eigen(a, symmetric = TRUE))
)) # 2.1 -> 2.8  # 233 sec
# 2.5 -> 3.2 -> 4.0
gc()

print(system.time(
  gc(test2 <- eigenArma(a@address))
)) # 1.2 -> 1.9 -> 3.4  # 233 sec
s <- rev(cols_along(test2$vectors))
test2$values <- drop(test2$values[s, ])
test2$vectors <- test2$vectors[, s]

plot(test$values, test2$values)
plot(test$vectors[, 1:10], test2$vectors[, 1:10])

