################################################################################

context("INCR_LOCK")

################################################################################

mat2 <- FBM(1, 1, init = 0)
expect_identical(mat2[], 0)

N <- round(runif(1, 100, 1000))

registerDoParallel(cl <- makeCluster(2))
test <- foreach(k = 1:N, .combine = 'c') %dopar% {
  bigstatsr::big_increment(mat2, k, use_lock = TRUE)
}
stopCluster(cl)

expect_identical(test, NULL)
expect_identical(mat2[], sum(1:N) + 0)

###############################################################################
