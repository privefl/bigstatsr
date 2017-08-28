X <- bigstatsr:::FBM.code256_RC$new(
  nrow = 15155,
  ncol = 281122,
  code = c(0, 1, 2, NA, 0, 1, 2, rep(NA, 249)),
  backingfile = "../paper1-packages/backingfiles/celiacQC",
  create_bk = FALSE
)

dim(X)
X[1:5, 1:5]
counts <- big_counts(X)
counts[, 1:10]

A <- matrix(0, nrow(X), 500); A[] <- rnorm(length(A))

system.time(
  test <- big_cprodMat(X, A)
) # 25 / 31 / 107

system.time(
  test2 <- big_cprodMat(X, A, ncores = 2)
) # 17 / 21 / 64
all.equal(test2, test)

system.time(
  test3 <- big_cprodMat(X, A, ncores = 6)
) # 15 / 21 / 36
all.equal(test3, test)

system.time(
  test4 <- big_cprodMat(X, A, ncores = 11)
) #    /    / 46
all.equal(test4, test)
