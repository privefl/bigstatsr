library(bigstatsr)
x <- datasets::airquality
X <- FBM(nrow(x), ncol(x))
Sys.sleep(1)
bigstatsr:::replaceDF(X$address, 1, 1:6, x[1, ])
X[]
