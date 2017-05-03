nrows <- c(3, 3, 4)
ind <- 1:10 - 1
ind2 <- rep(NA_integer_, length(ind))
for (i in ind) {
  k <- 1
  while (i >= nrows[k]) {
    k <- k + 1
    i <- i - nrows[k-1]
  }
  cat(k, i, "\n")
}
