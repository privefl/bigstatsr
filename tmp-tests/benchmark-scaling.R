#' Date: 2016-10-28
#' Object: Benchmark scalings
#' Results: Do not need to change code.

Rcpp::sourceCpp('src/PCA.cpp')

require(bigmemory)

N <- 1e3
M <- 20e3
a <- matrix(0, N, M)
a[] <- rnorm(length(a))
b <- as.big.matrix(a, type = "char", shared = FALSE)
means <- rnorm(M)
sds <- rnorm(M)
ind <- 1:1000 + 10e3

f1 <- function(b, means, sds, ind) {
  scaling(b[, ind], means[ind], sds[ind])
}

f2 <- function(b, means, sds, ind) {
  scaling2(b@address, means, sds, ind)
}

f3 <- function(b, means, sds, ind) {
  sweep(sweep(b[, ind], 2, means[ind], '-'), 2, sds[ind], '/')
}

require(microbenchmark)

print(microbenchmark(
  r1 <- f1(b, means, sds, ind),
  r2 <- f2(b, means, sds, ind),
  r3 <- f3(b, means, sds, ind),
  times = 100
))

# Unit: milliseconds
# expr       min        lq      mean    median        uq        max neval
# r1 <- f1(b, means, sds, ind)  5.150028  5.200522  5.313110  5.231602  5.268687   6.748764   100
# r2 <- f2(b, means, sds, ind)  7.603272  7.661904  7.755581  7.702831  7.757132   9.094733   100
# r3 <- f3(b, means, sds, ind) 12.939984 13.172591 14.578577 13.247844 13.397305 117.773008   100

print(all.equal(r1, r3))
print(all.equal(r2, r3))


# // [[Rcpp::export]]
# NumericMatrix scaling2(SEXP pBigMat,
#                        const NumericVector& mean,
#                        const NumericVector& sd,
#                        const IntegerVector& colInd) {
#   XPtr<BigMatrix> xpMat(pBigMat);
#   MatrixAccessor<char> macc(*xpMat);
#   int n = xpMat->nrow();
#   int m = colInd.size();
#
#   NumericMatrix res(n, m);
#   IntegerVector ind = colInd - 1;
#   int k;
#
#   for (int j = 0; j < m; j++) {
#     k = ind[j];
#     for (int i = 0; i < n; i++) {
#       res(i,j) = macc[k][i];
#       res(i,j) -= mean[k];
#       res(i,j) /= sd[k];
#     }
#   }
#
#   return(res);
# }
