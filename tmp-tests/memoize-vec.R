intervals <- lapply(1:10, function(i) {
  seq(100 * (i - 1) + 1, 100 * i)
})

mat <- matrix(0, 10000, 1000); mat[] <- rnorm(length(mat))


fun1 <- function(mat, ind) colSums(mat[, ind])

ind.intervals <- sample(1:10, size = 1e3, replace = TRUE)
system.time(
  test1 <- lapply(ind.intervals, function(i) {
    fun1(mat, intervals[[i]])
  })
) # 0.9 sec

library(memoise)
# forget(fun2 <- memoize(fun1))
# system.time(
#   test2 <- lapply(ind.intervals, function(i) {
#     fun2(mat, intervals[[i]])
#   })
# ) # 45 sec
# all.equal(test2, test1)

fun3 <- function(i) {
  fun1(mat, intervals[[i]])
}
forget(fun4 <- memoize(fun3))

system.time(
  test3 <- lapply(ind.intervals, function(i) {
    fun4(i)
  })
) # 0.1 sec
all.equal(test3, test1)
