require(bigstatsr)

X <- big_attach("../bigsnpr/backingfiles/celiac_impute1_sub1.desc")
dim(X)

A <- matrix(0, ncol(X), 100); A[] <- rnorm(length(A))

# R-10 -> R-100 -> MRO-100
print(system.time(
  test <- big_prodMat(X, A, use.Eigen = TRUE)
)) # 35 sec -> 99 -> 110

print(system.time(
  test2 <- big_prodMat(X, A, use.Eigen = FALSE)
)) # 60 sec -> 56 -> 54

print(system.time(
  test3 <- big_prodMat(X, A, use.Eigen = TRUE, block.size = 1e4)
)) # 41 sec -> 110 -> 107

print(system.time(
  test4 <- big_prodMat(X, A, use.Eigen = FALSE, block.size = 1e4)
)) # 45 sec -> 67 -> 56

print(system.time(
  test5 <- big_prodMat(X, A, use.Eigen = TRUE, block.size = 100)
)) # 36 sec -> 142 -> 134

print(system.time(
  test6 <- big_prodMat(X, A, use.Eigen = FALSE, block.size = 100)
)) # 38 sec -> 75 -> 58
