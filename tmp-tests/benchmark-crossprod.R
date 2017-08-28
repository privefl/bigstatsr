library(bigstatsr)

G <- FBM.code256(
  nrow = 15155,
  ncol = 281122,
  code = c(0, 1, 2, NA, 0, 1, 2, rep(NA, 249)),
  backingfile = "../paper1-packages/backingfiles/celiacQC",
  create_bk = FALSE
)

G2 <- big_transpose(G)
G2

# benchmarks 2000 / 5000
ind <- seq_len(5000)

system.time(
  cor1 <- big_cor(G2, ind.col = ind)
) # 33 / 191 sec

system.time(
  cor2 <- big_crossprodSelf(G2, fun.scaling = big_scale(), ind.col = ind)
) # 46 / 245 sec
all.equal(cor2 / (nrow(G2) - 1), cor1, check.attributes = FALSE)

system.time(
  cor3 <- big_crossprodSelf2(G2, fun.scaling = big_scale(), ind.col = ind)
) #    / 192 sec
all.equal(cor3, cor2)

