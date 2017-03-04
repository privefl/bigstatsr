require(bigstatsr)

X <- attach.big.matrix("../bigsnpr/backingfiles/celiacRAW.desc")
dim(X)
X <- sub.big.matrix(X, lastRow = 10, lastCol = 20)

code <- rep(NA_real_, 256); code[1:3] <- 3:5
X2 <- as.BM.code(X, code)

X[1:5, 1:5]
X2[1:5, 1:5]
X2[, 1:10]
X2[, 1]
X2[1, ]
X2[1:3, ]

X2[cbind(c(1, 1), c(2, 3))]

X[1, 2:3]

as.raw(X[cbind(c(1, 1), c(2, 3))])


x <- matrix(as.raw(sample(0:255, 100)), 10, 10)
class(x)
typeof(x)
X <- as.big.matrix(x, type = "raw")
ind <- cbind(1, 2:3)
ind
X[1, 2:3]
X[ind]
