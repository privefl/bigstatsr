library(testthat)
library(bigstatsr)

test_check("bigstatsr", filter = "spLinReg")
test_check("bigstatsr", filter = "spLogReg")
test_check("bigstatsr", filter = "sp(Lin|Log)Reg", invert = TRUE)
