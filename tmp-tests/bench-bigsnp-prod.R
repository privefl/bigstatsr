library(bigsnpr)

# snp_readBed("../Dubois2010_data/FinnuncorrNLITUK1UK3hap300_QC_norel.bed")
bigsnp <- snp_attach("../Dubois2010_data/FinnuncorrNLITUK1UK3hap300_QC_norel.rds")
G <- bigsnp$genotypes$copy(code = c(0, 1, 2, rep(0, 253)))

A <- matrix(rnorm(ncol(G) * 10), ncol = 10)
# (15155 x 281122) x (281122 x 10)
test <- big_prodMat(G, A)
# Old: 38 sec / 37 GB R memory
# New: 18-22 sec / 0 R memory


B <- matrix(rnorm(nrow(G) * 10), ncol = 10)
# (281122 x 15155) x (15155 x 10)
test2 <- big_cprodMat(G, B)
# Old: 52 sec / 36 GB R memory
# New: 48 sec / 0 R memory


# (5000 x 281122) x (281122 x 5000)
test3 <- big_tcrossprodSelf(G, ind.row = 1:5000)
# Old: 660-760 sec / 18 GB R memory
# New: 870-720 sec / 0.4 GB R memory


# (10000 x 15155) x (15155 x 10000)
test4 <- big_crossprodSelf(G, ind.col = 1:10000)
# Old: 630 sec / 3 GB R memory
