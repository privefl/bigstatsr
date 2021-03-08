library(bigsnpr)

celiac <- snp_attach("../Dubois2010_data/FinnuncorrNLITUK1UK3hap300_QC_norel.rds")
G <- celiac$genotypes$copy(code = c(0, 1, 2, 0, rep(NA, 252)))
rows <- rep(rows_along(G), each = 2)
cols <- rep(cols_along(G), each = 2)

Rcpp::sourceCpp('src/prodMatVec.cpp')
system.time(
  rowsums <- pMatVec4(G, rep(1, length(cols)), rows, cols, ncores = 4)
)
# 10.5 sec with 1 core / 4-5 sec with 4 cores
str(rowsums) # 300390 300390 300244 300244 299072

system.time(
  rowsums2 <- big_prodVec(G, rep(1, length(cols)), rows, cols, ncores = 4)
) # 10.5 sec with 1 core / 7 sec with 4 cores
all.equal(rowsums2, rowsums)


Rcpp::sourceCpp('src/prodMatVec.cpp')
system.time(
  colsums <- cpMatVec4(G, rep(1, length(rows)), rows, cols, ncores = 4)
)
# 11 sec to 3.5 sec with 4 cores

system.time(
  colsums2 <- big_cprodVec(G, rep(1, length(rows)), rows, cols, ncores = 4)
)
all.equal(colsums2, colsums)

system.time(
  colsums0 <- big_colstats(G, rows, cols, ncores = 4)$sum
) # 7 sec
all.equal(colsums0, colsums)


system.time(
  obj.svd <- big_randomSVD(G, big_scale(), k = 2, ncores = 4)
)
# Old: 59 sec with 4 cores

system.time(
  obj.svd2 <- big_randomSVD(G, big_scale(), k = 2, ncores = 1,
                            fun.prod = function(...) big_prodVec(..., ncores = 4),
                            fun.cprod = function(...) big_cprodVec(..., ncores = 4))
)
# New: 55-62 sec with 4 cores
all.equal(obj.svd2$d, obj.svd$d)


system.time(
  test <- bigcolvars(G, rows, cols, ncores = 4)
) # 18 sec -> 5 sec with 4 cores

system.time(
  test2 <- big_colstats(G, rows, cols, ncores = 4)
) # 18 sec -> 7 sec with 4 cores
all.equal(as.data.frame(test), test2)

