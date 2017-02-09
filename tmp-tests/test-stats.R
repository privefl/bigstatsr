require(bigmemory)

x <- as.big.matrix(
  matrix( sample(1:10, 20000, replace=TRUE), 5, 40000,
          dimnames=list( NULL, rep(c("a", "b", "c", "d"), 10000) ) ) )


print(system.time(
  test <- biganalytics::apply(x, 2, function(x) {sqrt(sum(x^2))})
))

print(system.time(
  test2 <- sqrt(biganalytics::apply(x, 2, function(x) {sum(x^2)}))
))

print(system.time(
  test3 <- sqrt(colSums(x[,]^2))
))
