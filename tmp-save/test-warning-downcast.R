library(bigstatsr)
options(bigstatsr.downcast.warning = FALSE)
FBM(10, 10, runif(100), type = "raw")
options(bigstatsr.downcast.warning = TRUE)
FBM(10, 10, runif(100), type = "raw")


x <- runif(100)
x[1] <- NA
FBM(10, 10, x, type = "raw")
FBM(10, 10, x, type = "unsigned short")
FBM(10, 10, x, type = "integer")[1]
FBM(10, 10, x, type = "double")

x2 <- 1:100
x2[1] <- NA
FBM(10, 10, x2, type = "raw")
FBM(10, 10, x2, type = "unsigned short")
FBM(10, 10, x2, type = "integer")[1]
FBM(10, 10, x2, type = "double")[1]

x3 <- sample(c(TRUE, FALSE), 100, TRUE)
x3[1] <- NA
FBM(10, 10, x3, type = "raw")
FBM(10, 10, x3, type = "unsigned short")
FBM(10, 10, x3, type = "integer")[1]
FBM(10, 10, x3, type = "double")[1]
