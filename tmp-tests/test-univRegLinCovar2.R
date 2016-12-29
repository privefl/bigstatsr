require(bigsnpr)
require(bigstatsr)

celiac <- AttachBigSNP("../bigsnpr/backingfiles/celiac_impute1_sub1.bk")
X <- celiac$genotypes
n <- nrow(X)
K <- 10
covar <- matrix(0, n, K)
covar[] <- rnorm(length(covar))

print(system.time(
  test <- big_univRegLin(X, rnorm(n), covar.train = covar)
))

str(test)
