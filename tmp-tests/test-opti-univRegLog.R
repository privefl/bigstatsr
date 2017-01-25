#' Date: 2017-01-17
#' Object: Test the knee heuristic on loss vs nb predictors
#' Results: Works well


require(bigsnpr)
require(bigstatsr)

# backingfile <- "../thesis-celiac/backingfiles/celiac_sub2_impute1.bk"
backingfile <- "../bigsnpr/backingfiles/celiac_impute1_sub1.bk"
celiac <- AttachBigSNP(backingfile)
X <- celiac$genotypes
y <- celiac$fam$affection - 1

X2 <- sub.big.matrix(X, firstCol = 1e5 + 1e4, lastCol = 1e5 + 3e4)
n <- nrow(X)

K <- 2
covar <- matrix(rnorm(n*K), n)

print(system.time(
  test <- big_univRegLog(X2, y, covar.train = covar, ncores = 4)
))
### current
# 40/42 sec with K = 2 -> niter = 3.7
# 114 sec with K = 10
### change
# 152 sec with K = 2 -> niter = 15+
plot(test$estim, readRDS("estim"))

## bim, 23 sec!
