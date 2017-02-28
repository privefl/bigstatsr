require(bigstatsr)

X <- big.matrix(10, 10, type = "raw")
X[,]

X2 <- as.BM.code(X, code = c(0, rep(NA, 255)))
X2[,]

y <- rnorm(10)
test <- big_prodVec(X, y) # error
test2 <- big_prodVec(X2, y)

X2@code[1] <- 1
test3 <- big_prodVec(X2, y)
print(test3 - sum(y))

X2.desc <- describe(X2)
test4 <- attach.BM(X2)
test4[,]
test5 <- attach.BM(X2.desc)
test5[,]

