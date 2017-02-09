# http://stackoverflow.com/questions/42111876/operating-with-big-matrix/42131766#42131766

require(bigmemory)

x <- as.big.matrix(
  matrix( sample(1:10, 20000, replace=TRUE), 5, 40000,
          dimnames=list( NULL, rep(c("a", "b", "c", "d"), 10000) ) ) )

print(system.time(
  true <- sqrt(colSums(x[,]^2))
))

print(system.time(
  test1 <- biganalytics::apply(x, 2, function(x) {sqrt(sum(x^2))})
))
print(all.equal(test1, true))



CutBySize <- function(m, block.size, nb = ceiling(m / block.size)) {
  int <- m / nb

  upper <- round(1:nb * int)
  lower <- c(1, upper[-nb] + 1)
  size <- c(upper[1], diff(upper))

  cbind(lower, upper, size)
}

seq2 <- function(lims) seq(lims["lower"], lims["upper"])

require(foreach)
big_aggregate <- function(X, FUN, .combine, block.size = 1e3) {
  intervals <- CutBySize(ncol(X), block.size)

  foreach(k = 1:nrow(intervals), .combine = .combine) %do% {
    FUN(X[, seq2(intervals[k, ])])
  }
}


print(system.time(
  test2 <- big_aggregate(x, function(X) sqrt(colSums(X^2)), .combine = 'c')
))
print(all.equal(test2, true))


