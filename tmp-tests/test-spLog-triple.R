library(bigsnpr)
celiac <- snp_attach("../paper-packages/backingfiles/celiacQC.rds")
G <- celiac$genotypes
y01 <- celiac$fam$affection - 1

ind.train <- sort(sample(length(y01), 10e3))

Rcpp::sourceCpp('tmp-tests/test-spLog-triple.cpp')
check_args <- bigstatsr:::check_args
assert_lengths <- bigstatsr:::assert_lengths
transform_levels <- bigstatsr:::transform_levels
library(Matrix)
system.time(
  test <- big_spLogReg(G, y01[ind.train], ind.train = ind.train)
)
