require(bigsnpr)
require(bigstatsr)

popres <- AttachBigSNP("../thesis-celiac/popres/backingfiles/popres_sub1.bk")
X <- popres$genotypes

X4 <- deepcopy(X, shared = FALSE)

print(system.time(
  test <- big_randomSVD(X4, snp_scaleBinom, k = 10,
                        verbose = TRUE, ncores = 1)
)) # 148 sec

# X <- deepcopy(X, type = "double", backingfile = "popres-double",
#               backingpath = "tmp-data/")

print(system.time(
  test <- big_randomSVD(X, snp_scaleBinom, k = 10,
                        verbose = TRUE, ncores = 4)
)) # 52 sec

X2 <- attach.big.matrix("tmp-data/popres-double.desc")

print(system.time(
  test2 <- big_randomSVD(X2, snp_scaleBinom, k = 10,
                         verbose = TRUE, ncores = 4)
)) # 64 sec

X3 <- attach.big.matrix("tmp-data/popres-int.desc")

print(system.time(
  test3 <- big_randomSVD(X3, snp_scaleBinom, k = 10,
                         verbose = TRUE, ncores = 4)
)) # 55 sec

