a <- matrix(1, 2e4, 2e4)
a[1] <- 2

library(bigstatsr)
X <- FBM(5e3, 50e3)
X[] <- rnorm(length(X))
X[1:5, 1:5]

y01 <- sample(0:1, size = nrow(X), replace = TRUE)

doParallel::registerDoParallel(1)
system.time(
  test1 <- big_univLogReg(X, y01)
)

doParallel::registerDoParallel(2)
system.time(
  test2 <- big_univLogReg(X, y01)
)

doParallel::registerDoParallel(3)
system.time(
  test2 <- big_univLogReg(X, y01)
)

doMC::registerDoMC(2)
system.time(
  test3 <- big_univLogReg(X, y01)
)

doMC::registerDoMC(3)
system.time(
  test4 <- big_univLogReg(X, y01)
)

doMC::registerDoMC(4)
system.time(
  test5 <- big_univLogReg(X, y01)
)

cl <- parallel::makeForkCluster(3)
doParallel::registerDoParallel(cl)
system.time(
  test6 <- big_univLogReg(X, y01)
)
parallel::stopCluster(cl)


cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)
system.time(
  test7 <- big_univLogReg(X, y01)
)
parallel::stopCluster(cl)


doParallel::registerDoParallel(4)
system.time(
  test8 <- big_univLogReg(X, y01)
)

doMC::registerDoMC(4)
system.time(
  test9 <- big_univLogReg(X, y01)
)

doParallel::registerDoParallel()
foreach::getDoParWorkers() # 2
options(cores = 3)
foreach::getDoParWorkers() # 3

system.time(
  test10 <- big_univLogReg(X, y01)
)

options(cores = 1)
system.time(
  test11 <- big_univLogReg(X, y01)
)

doParallel::registerDoParallel()
foreach::getDoParWorkers() # 1

for (i in 2:11) {
  stopifnot(eval(parse(text = sprintf("all.equal(test1, test%d)", i))))
}

