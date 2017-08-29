library(bigsnpr)

snp_readBed("../thesis-celiac/Dubois2010_data/FinnuncorrNLITUK1UK3hap300_QC_norel.bed",
            "test")

celiac <- snp_attach("test.rds")
G <- celiac$genotypes
G

G2 <- big_transpose(G, backingfile = "test3")
G2

# benchmarks for 1000 / 2000 / 5000 / not reducing block.size when par
ind <- seq_len(5000)

system.time(
  cor1 <- big_cor(G2, ind.col = ind)
) # 13 / 39 / 210 / 39 / 212 sec

system.time(
  cor2 <- big_cor(G2, ind.col = ind, ncores = 2)
) # 12 / 32 / 163 / 23 / 112 sec
all.equal(cor2, cor1)

system.time(
  cor3 <- big_cor(G2, ind.col = ind, ncores = 6)
) # 19 / 50 / 241 / 18 / 65 sec
all.equal(cor3, cor1)


# m <- ncol(G2)
# block.size <- block_size(nrow(G2))
# intervals <- bigstatsr:::CutBySize(m, block.size)
#
# ncores <- 1
# intervals2 <- bigstatsr:::CutBySize(m, nb = (ncores * 2 - 1) * nrow(intervals))
# all.equal(intervals, intervals2)
#
# ncores <- 6
# intervals <- bigstatsr:::CutBySize(m, nb = (ncores * 2 - 1) * nrow(bigstatsr:::CutBySize(m, block.size)))
