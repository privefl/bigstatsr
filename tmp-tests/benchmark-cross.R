# WTF: Eigen not faster anymore?

require(bigstatsr)

X <- attach.big.matrix("../bigsnpr/backingfiles/popres.desc")

print(system.time(
  test <- big_tcrossprodSelf(X, big_scale(), use.Eigen = TRUE)
)) # 73 sec

print(system.time(
  test2 <- big_tcrossprodSelf(X, big_scale(), use.Eigen = FALSE)
)) # 35 sec -> 27

X2 <- big_transpose(X, descriptor = FALSE, shared = FALSE)

print(system.time(
  test3 <- big_crossprodSelf(X2, big_scale(), use.Eigen = TRUE)
)) # 108 sec

print(system.time(
  test4 <- big_crossprodSelf(X2, big_scale(), use.Eigen = FALSE)
)) # 41 sec -> 40
