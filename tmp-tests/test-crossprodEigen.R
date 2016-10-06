#' Date: 2016-10-06
#' Object: Testing crossprodEigen because it doesn't work.
#' And determine if using 'adjoint' is faster than 'transpose'.
#' Results: sub.big.matrix doesn't work with Map<> de Eigen.

Rcpp::sourceCpp('src/PCA.cpp')

a <- big.matrix(10, 10)
b <- matrix(rnorm(50), 5, 10)
test <- crossprod(b, b)
dim(test)
test[1:5, 1:5]
crossprodEigen2(a@address, b, b)
a[1:5, 1:5]
crossprodEigen3(a@address, b, b)
a[1:5, 1:5]


b.sub <- b[, 1:3]

a.sub <- sub.big.matrix(a, 1, 3, 1, 3)
a.sub[] <- 0
crossprodEigen2(a.sub@address, b.sub, b.sub)
a.sub[,]
a[,]

a.sub[] <- 0
crossprodEigen3(a.sub@address, b.sub, b.sub)
a.sub[,]
a[,]

test <- crossprodEigen4(b.sub, b.sub)
