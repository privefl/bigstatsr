#' Date: 2017-02-13
#' Object: Debugg big_spRegLog with covariates
#' Results: Replace assert by Rccp exception

require(bigstatsr)
X <- big.matrix(500, 200)
X[] <- rnorm(length(X))

covar <- matrix(0, nrow(X), 10)
covar[] <- rnorm(length(covar))

y <- sample(0:1, nrow(X), TRUE)

test <- big_spRegLog(X, y, covar.train = covar)
