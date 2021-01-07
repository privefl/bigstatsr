library(bigsnpr)

celiac <- snp_attach("../Dubois2010_data/FinnuncorrNLITUK1UK3hap300_QC_norel.rds")
G <- celiac$genotypes$copy(code = c(0, 1, 2, 0, rep(NA, 252)))
rows <- rep(rows_along(G), each = 1)
cols <- rep(cols_along(G), each = 2)

n <- length(rows)
covar <- matrix(rnorm(n * 3), n, 3)
U <- svd(cbind(1, covar), nv = 0)$u
y <- rnorm(n)

Rcpp::sourceCpp('src/univRegLin5.cpp')
system.time(
  gwas1 <- univLinReg5(G, U, y, rows, cols, ncores = 4)
)
# 24 / 7.5 (4 cores)
# cols x2 - 4 cores: 19 - 6 cores: 14.5 - 20 cores: 11
str(gwas1)

options(bigstatsr.ncores.max = 20)
system.time(
  gwas2 <- big_univLinReg(G, y, ind.train = rows, ind.col = cols,
                          covar.train = covar, ncores = 4)
)
# 27 / 13
# cols x2 - 4 cores: 22 - 6 cores: 19.5 - 20 cores: 23 sec
head(gwas2, 4)
