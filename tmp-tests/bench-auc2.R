x <- rnorm(1e6)
y <- sample(0:1, size = length(x), replace = TRUE)
# system.time(bigstatsr::AUCBoot(x, y, nboot = 1e3))
system.time(AUCBoot(x, y, nboot = 1e3))
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
Rcpp::sourceCpp('tmp-tests/fast-sort-sample.cpp')
library(dqrng)
microbenchmark::microbenchmark(
  sort(sample(n, replace = TRUE)),
  tabulate(sample(n, replace = TRUE), n),
  sort(dqsample.int(n, replace = TRUE)),
  tabulate(dqsample.int(n, replace = TRUE)),
  sort_sample(n)
)

AUCBoot <- function(pred, target, nboot = 1e4, seed = NA, digits = NULL) {

  y <- as.logical(target)

  ord <- order(pred, y)
  pred <- pred[ord]
  y <- y[ord]

  if (!is.na(seed)) {
    # http://stackoverflow.com/a/14324316/6103040
    old <- .Random.seed
    on.exit( { .Random.seed <<- old } )
    set.seed(seed)
  }

  repl <- boot_auc_sorted_tab(pred, y, nboot)

  if (nbNA <- sum(is.na(repl)))
    warning2("%d/%d bootstrap replicates were mono-class.", nbNA, nboot)

  res <- c("Mean" = mean(repl, na.rm = TRUE),
           stats::quantile(repl, c(0.025, 0.975), na.rm = TRUE),
           "Sd" = stats::sd(repl, na.rm = TRUE))

  round2(res, digits)
}

system.time(print(AUCBoot(x, y, nboot = 1e3)))
