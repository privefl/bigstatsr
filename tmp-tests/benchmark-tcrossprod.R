#' Date: 2016-10-04
#' Object: Benchmark computing X*X^T on Celiac
#' Results: Check BigXtX which is very slow. OK now.
#' But is faster with use.Eigen. And way faster with MRO.

library(bigsnpr)
library(bigstatsr)

test <- AttachBigSNP("celiac_impute3", "../thesis-celiac/backingfiles")

# 87 min with Eigen.
# 6 min with MRO.
print(system.time(test2 <- BigXYt(test$genotypes, block.size = 1e4,
                                  use.Eigen = TRUE)))

test3 <- transpose(test$genotypes, shared = FALSE)

# 100 min with Eigen.
# 27 min with MRO. Bad choise of block.size = 200? Because only 20% CPU
# 14 min with MRO and block.size = 500.
print(system.time(test4 <- BigXtX(test3, block.size = 500,
                                  use.Eigen = TRUE)))
