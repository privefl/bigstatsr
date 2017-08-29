l <- readRDS("debug.rds")
do.call(bigstatsr::big_CMSA, args = l)

ind.train <- sort(unique(as.integer(unlist(
  lapply(l, function(li) rownames(li$scores))
))))

feval <- bigstatsr::AUC

for (i in 1:1000) {
  y <- sample(0:1, size = 911, replace = TRUE)
  y.train <- y[ind.train]

  betas <- sapply(l, function(x) {
    tmp <- x$scores
    ind <- as.numeric(rownames(tmp))
    stopifnot(all(ind %in% ind.train))
    ind2 <- match(ind, ind.train)
    seval <- apply(tmp, 2, feval, target = y.train[ind2])
    x$betas[, which.max(seval)]
  })
}

