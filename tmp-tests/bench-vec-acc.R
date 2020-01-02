library(bigstatsr)
X <- FBM(10000, 10000, init = 1)
system.time(X[1:99999999])     # 1.7 -> 1.0 -> 2.5?
system.time(X[1:99999999 + 0]) # 1.7 -> 1.0 -> 2.5?
