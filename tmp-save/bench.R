library(bigstatsr)
X <- as_FBM(matrix(rnorm(1e7), 1e4))
bigstatsr:::assert_noNA(X)
system.time(big_randomSVD(X, big_scale(), k = 10))
# 2.3 - 3.2 sec with v0.9.1 (CRAN) and v0.9.10 (GitHub @master)
# 10 - 11 sec with GitHub @splitpkg
