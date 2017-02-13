# ftp://ftp.ncbi.nih.gov/pub/geo/DATA/supplementary/series/GSE6532/GSE6532_LUMINAL.RData.gz

load("~/Documents/LUMINAL.RData")
data.tam[is.na(data.tam)] <- 0
n <- nrow(data.tam)
m <- ncol(data.tam)

require(bigstatsr)
X <- as.big.matrix(rbind(data.tam, data.untreated),
                   backingfile = "LUMINAL.bk",
                   descriptorfile = "LUMINAL.desc",
                   backingpath = "tmp-data")

X <- attach.big.matrix("tmp-data/LUMINAL.desc")
dim(X)
y <- c(rep(0, 277), rep(1, 137))

print(system.time(
  true <- svd(scale(X[,]), nu = 10, nv = 10)
))

print(system.time(
  test <- big_randomSVD(X, big_scale())
))

plot(test$u, true$u)
abline(0, -1, col = "red")
abline(0, 1, col = "red")
plot(test$u, col = y + 1)

test2 <- big_univRegLog(X, y, covar.train = test$u)
plot(-log10(test2$p.value), pch = 19, cex = 0.5)

test3 <- big_spRegLogCV2(X, y, covar.train = test$u)
