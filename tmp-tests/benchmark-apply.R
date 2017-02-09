require(bigstatsr)
require(bigsnpr)

X <- snp_attach("../thesis-celiac/backingfiles/celiac_impute1.bk")$genotypes

print(system.time(
  test <- big_apply(X, colMeans, .combine = 'c')
)) # 68 sec

print(system.time(
  test2 <- big_apply(X, colMeans, .combine = 'c', ncores = 11)
)) # 8 sec

print(system.time(
  true <- big_colstats(X)$sum / nrow(X)
)) # 22 sec

print(all.equal(test, true))
print(all.equal(test2, true))
