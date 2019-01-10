
library(bigstatsr)

# nr <- 3e4; nc <- 2e5
# chi2.fbm <- FBM(nr, nc, backingfile = "tmp-data/test")$save()
#
# big_apply(chi2.fbm, a.FUN = function(X, ind) {
#   X[, ind] <- rchisq(nrow(X) * length(ind), 1)
#   NULL
# }, a.combine = 'c', ncores = nb_cores())

chi2.fbm <- big_attach("tmp-data/test.rds")
hist(chi2.fbm[, 1])
nr <- nrow(chi2.fbm)
nc <- ncol(chi2.fbm)

res.fbm <- FBM(nr, nc, backingfile = "tmp-data/test2")

# Funciton that will be used
DF <- rep(1, nr)
hist(qnorm(pchisq(chi2.fbm[, 1], df = DF)))
chi2z.par <-  function(X, ind, df, Out.mat) {
  Out.mat[, ind] <- qnorm(pchisq(X[, ind], df = df))
  NULL
}

# Fill with big_apply
system.time(
  big_apply(chi2.fbm, a.FUN = chi2z.par, ncores = nb_cores(),
            df = DF, Out.mat = res.fbm)
) # 21 min  //  36 MB/s

ind <- sample(nc, 100)
test <- res.fbm[, ind]

system.time({
  col.block <- 10000
  last.beg <- ncol(res.fbm)
  for (beg in seq(1, last.beg, by=col.block)) {
    end <- beg + col.block - 1
    end <- min(end, ncol(res.fbm))
    cols <- beg:end
    tm <- system.time(
      empty <- big_parallelize(chi2.fbm, p.FUN = chi2z.par,
                               ind = cols, ncores = nb_cores(),
                               df = DF, Out.mat = res.fbm)
    )
    cat(tm[3], "\n")
  }
}) # 18 min

stopifnot(identical(res.fbm[, ind], test))

system.time(trans_cpp(chi2.fbm, res.fbm, DF))

stopifnot(identical(res.fbm[, ind], test))
