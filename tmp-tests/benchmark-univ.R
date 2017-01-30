require(bigsnpr)
require(bigstatsr)

# celiac <- AttachBigSNP("../bigsnpr/backingfiles/celiac_impute1_sub1.bk")
celiac <- AttachBigSNP("../thesis-celiac/backingfiles/celiac_sub2_impute1.bk")
X <- celiac$genotypes
N <- nrow(X)
y <- rnorm(N)

K <- 10
covar <- matrix(rnorm(N*K), N)

print(system.time(
  test <- big_univRegLin(X, y, covar.train = covar, ncores = 7)
))
# 19 sec with 3 cores
# 13 sec with 5 cores
# 11 sec with 6 cores
# 13 sec with 7 cores
# 13 sec with 10 cores

# print(system.time(
#   test2 <- big_univRegLog(X, celiac$fam$affection-1, covar.train = covar, ncores = 10)
# ))
# 323 sec with 6 cores
# 516 sec with 10 cores?

print(system.time(
  test3 <- big_randomSVD(X, big_scale(), verbose = TRUE, ncores = 10)
))
# 192 sec with 6 cores
# 128 sec with 10 cores
