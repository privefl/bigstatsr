# Get some infos on the dataset at
# http://www.cise.ufl.edu/research/sparse/matrices/NYPA/Maragal_7.html

require(bigstatsr)

desc <- "sparse.desc"
bk <- sub("\\.desc$", ".bk", desc)
if (file.exists(bk)) {
  X <- attach.big.matrix(desc)
} else {
  # read the data
  url <- "http://www.cise.ufl.edu/research/sparse/mat/NYPA/Maragal_7.mat"
  file <- R.matlab::readMat(url)
  x <- file$Problem[[3]]
  d <- x@Dim
  stopifnot(d[2] == 116*229)
  # store it in a filebacked big.matrix
  X <- big.matrix(d[1], d[2], backingfile = bk, descriptorfile = desc)
  for (k in 1:229) {
    ind <- 1:116 + (k - 1) * 116
    X[, ind] <- x[, ind]
  }
}

sds <- big_apply(X, function(x) apply(x, 2, sd),
                 .combine = 'c', block.size = 100)
ind.keep <- which(sds > 1e-3)
print(length(ind.keep))

print(system.time(
  test <- big_randomSVD(X, big_scale(center = TRUE, scale = TRUE),
                        ind.col = ind.keep,
                        k = 10, verbose = TRUE)
))
plot(test$u)
plot(test$v[, 1], type = "h")
plot(test$v[, 1], sds[ind.keep])
