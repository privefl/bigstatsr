require(bigstatsr)

X <- attach.big.matrix("../bigsnpr/backingfiles/celiac_")
dim(X)

y <- rnorm(ncol(X))

X2 <- deepcopy(X, type = "raw", backingfile = "celiacRAW",
               backingpath = "../bigsnpr/backingfiles/")
X2[1:5, 1:5]

print(system.time(
  test <- pMatVec4(X@address, y, 1:nrow(X), 1:ncol(X), lookup = numeric(0))
))

lookup <- rep(NA_real_, 256)
lookup[1:3] <- 0:2
print(system.time(
  test2 <- pMatVec4(X2@address, y, 1:nrow(X), 1:ncol(X), lookup = lookup)
))


save <- X2[1, 1]
X2[1,1] <- 2*save + 4

print(system.time(
  true <- big_apply(X, a.FUN = function(x, ind) x[, ind] %*% y[ind],
                    a.combine = '+')
))
ind <- which(is.na(true))
true.ind <- X[ind, ] %*% y
ind2 <- which(is.na(X[ind, ]), arr.ind = TRUE)


print(system.time(
  true <- big_apply(X, a.FUN = function(x, ind) x[, ind] %*% y[ind],
                    a.combine = '+')
))

print(system.time(
  true2 <- big_apply(X2, a.FUN = function(x, ind, lookup) {
    access(x[, ind], lookup) %*% y[ind]
  }, a.combine = '+', lookup = lookup)
))

access(X2[, 1:100], lookup)
