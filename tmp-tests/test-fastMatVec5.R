require(bigsnpr)
require(bigstatsr)

celiac <- AttachBigSNP("../bigsnpr/backingfiles/celiac_impute1_sub1.bk")
X <- celiac$genotypes

# X <- deepcopy(X, type = "double", backingfile = "celiac-double",
#               backingpath = "tmp-data/")

print(system.time(
  test <- big_randomSVD(X, snp_scaleBinom, k = 10,
                        verbose = TRUE, ncores = 4)
)) # 166 sec
#
# BigToBed(celiac, "tmp-data/celiac.bed")
#
# require(flashpcaR)
# print(system.time(
#   testFlash <- flashpca("tmp-data/celiac", ndim = 10,
#                         stand = "binom2", do_loadings = TRUE)
# )) # 589 sec
# plot(test$u, testFlash$vectors)
# s <- c(rep(FALSE, 29), TRUE)
# plot(test$v[s], testFlash$loadings[s])

x1 <- rnorm(ncol(X))
x2 <- rnorm(nrow(X))

print(system.time(
  test3 <- produ3(X@address, x1)
)) # 10- sec

print(system.time(
  test5 <- produ4(X@address, x1)
)) # 10- sec

print(system.time(
  test1 <- produ2(X@address, x1)
)) # 17 sec
all.equal(test3, test1)

print(system.time(
  test4 <- crossprodu3(X@address, x2)
)) # 5.7 sec

print(system.time(
  test2 <- crossprodu2(X@address, x2)
)) # 11.5 sec
all.equal(test2, test4)

require(bigmemory)
X <- big.matrix(3, 5, type = "char")
X.desc <- describe(X)
X2 <- attach.big.matrix(X.desc, readonly = TRUE)
X[] <- 1:15
x1 <- rnorm(5)
print(system.time(
  test1 <- produ2(X2@address, x1)
)) # 17 sec
