library(bigstatsr)
options(bigstatsr.downcast.warning = FALSE)

N <- M <- 2000
size <- N * M * 8 / 1024^2
x4 <- matrix(round(rnorm(N * M, 100, 10)), N)
x3 <- x2 <- x1 <- x4
storage.mode(x1) <- "raw"
storage.mode(x2) <- "logical"
storage.mode(x3) <- "integer"

X4 <- big_copy(x4, type = "double")
X3 <- big_copy(x4, type = "integer")
X2 <- big_copy(x4, type = "unsigned short")
X1 <- big_copy(x4, type = "unsigned char")

print(size)
for (X in list(X1, X2, X3, X4)) {
  print(typeof(X))
  for (x in list(x1, x2, x3, x4)) {
    tmp <- gc(reset = TRUE)
    X[] <- x
    print((gc() - tmp)[2, 6])
    # print(size / 10)
  }
}
