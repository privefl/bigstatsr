a <- matrix(runif(1e8), 1e4, 1e4)
b <- float::as.float(a)

microbenchmark::microbenchmark(
  base::rowSums(a),
  float::rowSums(b)
)


a <- float::float(10)
a
a[1] <- NA_float_
a

a[1] + 1L
a / 0


a <- float::float(10)
a[2] <- as.integer(2^26)
a[2]
a[2] + 1L

b <- integer(2)
b[2] <- 3


a <- matrix(runif(1e9), 1e5)
a[1] <- NA
system.time(float::as.float(a))
system.time(bigstatsr::without_downcast_warning(bigstatsr::as_FBM(a, type = "float")))
