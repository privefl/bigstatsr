source('tmp-save/randomSVD.R', encoding = 'UTF-8')
Rcpp::sourceCpp('src/mult.cpp')
Rcpp::sourceCpp('src/PCA.cpp')
Rcpp::sourceCpp('src/randomProjPCA.cpp')


# Simulating some data
N <- 1501
M <- 781
x <- matrix(rnorm(N*M, sd = 5), N)
X <- as.big.matrix(x, type = "char", shared = TRUE)

library(bigstatr)

ind <- sort(sample(N, N/2))
test <- big_randomSVD2(X, big_scale(), ind.train = ind)
plot(test$v)

test2 <- big_randomSVD(X, big_scale(), ind.train = ind,
                       block.size = 10, ncores = 3)
plot(test$v, test2$v)
