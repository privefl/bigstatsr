require(bigsnpr)

# celiac <- AttachBigSNP("../bigsnpr/backingfiles/celiac_impute1_sub1.bk")
celiac <- AttachBigSNP("../thesis-celiac/backingfiles/celiac_sub2_impute1.bk")
X <- celiac$genotypes
X2 <- sub.big.matrix(X, lastCol = 100e3)
# print(dim(X2))
N <- nrow(X)
y <- rnorm(N)

Rcpp::sourceCpp('tmp-tests/univRegLin4.cpp')
Rcpp::sourceCpp('src/univRegLin2.cpp')
Rcpp::sourceCpp('tmp-tests/univRegLin3.cpp')
Rcpp::sourceCpp('src/univRegLin.cpp')
Rcpp::sourceCpp('tmp-tests/univRegLin5.cpp')

K <- 20
ind.train <- sort(sample(N, 20000, TRUE))
covar <- cbind(1, matrix(rnorm(N*K), N))


print(system.time(
  current <- univRegLin2(X2@address, cbind(0, covar)[ind.train, ], y[ind.train], ind.train)
))

print(system.time(
  change1 <- univRegLin3(X2@address, covar[ind.train, , drop = FALSE],
                         y[ind.train], ind.train)
))

# print(system.time(
#   change2 <- univRegLin4(X2@address, y[ind.train], svd(covar[ind.train, ]), ind.train)
# ))

SVD <- svd(covar[ind.train, ], nv = 0)
K2 <- sum(SVD$d / sqrt(length(ind.train)) > 1e-3)
print(mean(SVD$d) / sqrt(length(ind.train)))
print(system.time(
  change3 <- univRegLin5(X2@address, SVD$u[, 1:K2, drop = FALSE], y[ind.train], ind.train)
))

if (K == 0) {
  print(system.time(
    current2 <- univRegLin(X2@address, y[ind.train], ind.train)
  ))
  print(all.equal(current$estim, current2$estim))
  print(all.equal(current$std.err, current2$std.err))
}

# plot(change$estim, current$estim); abline(0, 1, col = "red")
print(all.equal(current$estim, change1$estim))
print(all.equal(current$std.err, change1$std.err))
# print(all.equal(current$estim, change2$estim))
# print(all.equal(current$std.err, change2$std.err))
print(all.equal(current$estim, change3$estim))
print(all.equal(current$std.err, change3$std.err))
