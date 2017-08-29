library(bigstatsr)
X <- FBM(10e3, 20e3)
X[] <- rnorm(length(X))
X[1:5, 1:5]

doParallel::registerDoParallel()
options(cores = 1)
system.time(
  test1 <- big_randomSVD(X, big_scale())
) # 73

options(cores = 2)
system.time(
  test2 <- big_randomSVD(X, big_scale())
) # 68

options(cores = 3)
system.time(
  test3 <- big_randomSVD(X, big_scale())
) # 66

options(cores = 4)
system.time(
  test4 <- big_randomSVD(X, big_scale())
) # 66

all.equal(test1, test2)
all.equal(test2, test3)
all.equal(test3, test4)

options(cores = 1)
system.time(
  test1 <- big_randomSVD(X, big_scale(), verbose = TRUE)
)

options(cores = 2)
system.time(
  test2 <- big_randomSVD(X, big_scale(), verbose = TRUE)
) # 71

options(cores = 3)
system.time(
  test3 <- big_randomSVD(X, big_scale(), verbose = TRUE)
) # 72

options(cores = 4)
system.time(
  test4 <- big_randomSVD(X, big_scale(), verbose = TRUE)
) # 75
