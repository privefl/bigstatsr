D <- matrix(0, 21, 21)
diag(D) <- 1:21

D[, 21] <- rnorm(21)
true <- svd(D)
test <- getSVD(D)
plot(test$u, true$u)
plot(test$v, true$v)

begin <- Sys.time()
for (i in 1:5e5) {
  D[, 1] <- rnorm(21)
  test <- getSVD(D)
}
print(Sys.time() - begin)
# only 29 sec with K = 10
# 93 sec with K = 20
