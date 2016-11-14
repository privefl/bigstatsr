require(bigmemory)
a <- big.matrix(4, 4)
a[] <- 1:16
a[,]

b <- sub.big.matrix(a, firstCol = 2)
b[,]
c <- sub.big.matrix(b, firstRow = 2)
c[,]
d <- sub.big.matrix(c, lastCol = 1)
d[,]
