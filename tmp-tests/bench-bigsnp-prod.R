library(bigsnpr)

# snp_readBed("../Dubois2010_data/FinnuncorrNLITUK1UK3hap300_QC_norel.bed")
bigsnp <- snp_attach("../Dubois2010_data/FinnuncorrNLITUK1UK3hap300_QC_norel.rds")
G <- bigsnp$genotypes$copy(code = c(0, 1, 2, rep(0, 253)))

A <- matrix(rnorm(ncol(G) * 10), ncol = 10)

# (15155 x 281122) x (281122 x 10)
test <- big_prodMat(G, A)
# Old: 38 sec / 37 GB R memory
# New: 18-22 sec / 0 R memory

