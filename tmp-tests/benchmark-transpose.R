#' Date: 2016-10-04
#' Object: Benchmark "transpose" on Celiac
#' Results: 16 sec -> very fast

library(bigsnpr)
library(bigstatsr)

test <- AttachBigSNP("celiac", "../thesis-celiac/backingfiles")

print(system.time(test2 <- transpose(test$genotypes, shared = FALSE)))

print(dim(test2))

print(test2[1:10, 1:10] - t(test$genotypes[1:10, 1:10]))

print(test2[528969 - 0:10, 11950 - 0:10] -
        t(test$genotypes[11950 - 0:10, 528969 - 0:10]))

