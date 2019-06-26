x <- rnorm(1e5)
y <- sample(0:1, size = length(x), replace = TRUE)
system.time(bigstatsr::AUCBoot(x, y, nboot = 1e4))
# 1e5 / 1e3:  10 ->  1.6
# 1e6 / 1e3: 153 -> 23
# 1e5 / 1e4: 100 -> 15

y2 <- as.logical(y)
y3 <- as.factor(y)
x2 <- sample(seq_along(x))
x3 <- as.factor(x2)

microbenchmark::microbenchmark(
  order(x),
  order(x2),
  order(x3),
  xtfrm(x),
  order(x, y),
  order(x, y, method = "shell"),
  order(x, y, method = "radix"),
  order(x, y2),
  order(x, y2, method = "shell"),
  order(x, y2, method = "radix"),
  order(x, y3),
  order(x, y3, method = "shell"),
  order(x, y3, method = "radix")
)

z <- runif(8)
z
(ind <- order(z))

ind2 <- sample(length(z), replace = TRUE)
ind2

microbenchmark::microbenchmark(
  ind <- sample(1e6, replace = TRUE),
  ind2 <- runif(1e6, min = 1, max = 1e6 + 1)
)

microbenchmark::microbenchmark(
  ind2 <- sort(ind)
)

tabulate(sample(n, replace = TRUE), n)
n <- 1e6
microbenchmark::microbenchmark(
  sort(sample(n, replace = TRUE)),
  tabulate(sample(n, replace = TRUE), n),
  sort_sample(n)
)
