library(bigmemory)

x <- big.matrix(10, 10)
x[] <- 1:100
x[, 1:2]
x[, 1:2] <- 1:2
x[, 1:2]
x[, 1:2] <- t(1:2)

val <- t(1:2)
x[1, ] <- 1:2

file <- "tmp-tests/FBM/test.bin"
test <- FBM_class$new(file, 10, 10, "double")

readBin("../FBM/tmp-tests/test.bin", "double", 200)
dim(test)
test[] <- 1:99
test[] <- 1:100
readBin("../FBM/tmp-tests/test.bin", "double", 200)
test[] <- 2
test[, 1] <- 3
test[1, ] <- 4
test[c(FALSE, TRUE), ] <- 5
test[1:10] <- 1:10
test[cbind(c(1, 2), c(1, 1))] <- NA

test[1:2, 1:2] <- 0
readBin(file, "double", 200)
test[1:2, 1:2] <- 1:4

test[99:101] <- 1:3

test[]
test[,]
test[1:2]
test[1:2, ]
test[1, ]
test[, 1]
test[c(TRUE, FALSE), ]
test[-1, ]
