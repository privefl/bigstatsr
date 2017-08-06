Rcpp::sourceCpp('tmp-tests/test-pca-triple.cpp')
library(bigsnpr)
celiac <- snp_attach("../paper-packages/backingfiles/celiacQC.rds")
G <- celiac$genotypes
CHR <- celiac$map$chromosome
POS <- celiac$map$physical.pos
ind.excl <- snp_indLRLDR(CHR, POS)
system.time(
  ind.keep <- snp_clumping(G, CHR, exclude = ind.excl, ncores = 2)
)
system.time(
  obj.svd <- svds4.seq(G, big_scaleTriple, rows_along(G), ind.keep,
                       k = 10, tol = 1e-4, verbose = TRUE)
)


obj.svd2 <- structure(obj.svd, class = "big_SVD")
plot(obj.svd2) # same as normal
plot(obj.svd2, type = "scores", scores = 7:8)
