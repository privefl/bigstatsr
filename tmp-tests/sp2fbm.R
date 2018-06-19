# Packages
library(Matrix)
library(bigstatsr)

# Data
spMat <- sparseMatrix(i = integer(), j = integer(), dims = c(1e6, 2e3))
N <- 1e8
x <- runif(N)
i <- sample(nrow(spMat), N, replace = TRUE)
j <- sample(ncol(spMat), N, replace = TRUE)
spMat[cbind(i, j)] <- x

# Solutions
system.time(
  fbm <- FBM(nrow(spMat), ncol(spMat), init = 0)
) # 13 sec
file.size(fbm$backingfile) / 1024^3  # 15 GB

# All at once
system.time({
  ind_nozero <- which(spMat != 0, arr.ind = TRUE)
  fbm[ind_nozero] <- spMat[ind_nozero]
}) # 37 sec

# By blocks to use less memory
system.time(
  big_apply(fbm, a.FUN = function(X, ind, spMat) {
    offset <- min(ind) - 1
    ind_nozero <- which(spMat[, ind] != 0, arr.ind = TRUE)
    ind_nozero[, 2] <- ind_nozero[, 2] + offset
    X[ind_nozero] <- spMat[ind_nozero]
    NULL
  }, a.combine = 'c', spMat = spMat, block.size = 1e3)
) # 40 sec

all.equal(fbm[, 1500], spMat[, 1500])
