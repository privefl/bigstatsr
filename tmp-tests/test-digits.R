library(bigstatsr)

file <- "../train.csv"

(tmp <- readLines(file, 5))

test <- big_readBM(file,
                   file.nheader = 1,
                   info.nelem = 1,
                   split = ",",
                   read.what = integer(),
                   read.transfo = as.raw,
                   BM.type = "raw",
                   transpose = TRUE,
                   backingfile = "digits.bk",
                   backingpath = ".")
label <- as.integer(attr(test, "info"))
attr(test, "header")
pixel <- expand.grid(i = 0:27, j = 0:27)

test <- as.BM.code(test, code = as.double(0:255))
df <- big_scale()(test)

test <- deepcopy(test, cols = -ind, backingfile = "digits_sub.bk", backingpath = ".")

test <- as.BM.code(big_attach("digits_sub.bk.desc"), code = as.double(0:255))

library(ggplot2)

qplot(x = df$sd)
summary(df$sd)
ind <- which(df$sd < 1e-4)
ind.col <- setdiff(cols_along(test), ind)
svd <- big_SVD(test, big_scale(), k = 50)
plot(svd)
plot(svd, type = "scores") + aes(color = as.factor(label))



library(bigsnpr)
obj.pcadapt <- snp_pcadapt(test, svd$u)
