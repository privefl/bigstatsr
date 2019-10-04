mat <- FBM(1, 1, init = 0)
mat[]

## [1] 0

library(doParallel)
cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
foreach(k = 1:10, .combine = 'c') %dopar% {
  mat[1, 1] <- mat[1, 1] + k
  NULL
}
parallel::stopCluster(cl)
mat[]

## [1] 34

sum(1:10)

## [1] 55

mat2 <- FBM(1, 1, init = 0)
mat2[]

## [1] 0

cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
foreach(k = 1:10, .combine = 'c') %dopar% {
  bigstatsr::big_increment(mat2, k, use_lock = TRUE)
}
parallel::stopCluster(cl)
mat2[]

microbenchmark::microbenchmark(
  LOCK_ON  = bigstatsr::big_increment(mat2, 1, use_lock = TRUE),
  LOCK_OFF = bigstatsr::big_increment(mat2, 1, use_lock = FALSE),
  times = 1000
)
# Unit: microseconds
#     expr     min      lq     mean   median      uq      max neval
#  LOCK_ON 202.426 210.936 241.5740 217.1910 233.201 3514.899  1000
# LOCK_OFF  58.046  61.942  68.4772  64.7925  68.814  220.170  1000
