library(bigstatsr)

tmp <- tempfile()
X <- FBM(10, 10, backingfile = tmp, init = NA)$save()

file.info(paste0(tmp, ".bk"))[["mode"]]
X <- big_attach(paste0(tmp, ".rds"))
X[]
X[] <- 1

X$is_read_only <- TRUE
X[] <- 2

Sys.chmod(paste0(tmp, ".bk"), "0444")  ## make it read-only
file.info(paste0(tmp, ".bk"))[["mode"]]
X <- big_attach(paste0(tmp, ".rds"))
X[]
X[] <- 3
Sys.chmod(paste0(tmp, ".bk"), "0666")  ## make it read-only
X[] <- 4
