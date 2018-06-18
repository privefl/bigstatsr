// [[Rcpp::depends(BH, bigstatsr)]]
#include <bigstatsr/BMAcc.h>

template <class C>
double sum_mat_like(C macc) {
  
  size_t i, j, n = macc.nrow(), m = macc.ncol();
  double sum = 0;
  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      sum += macc(i, j);
    }
  }
  
  return sum;
}

// [[Rcpp::export]]
double sum_mat(const NumericMatrix& x) {
  return sum_mat_like(x);
}

// [[Rcpp::export]]
double sum_FBM(Environment x) {
  
  XPtr<FBM> xptr = x["address"];
  BMAcc<double> macc(xptr);
  
  return sum_mat_like(macc);
}

// [[Rcpp::export]]
double sum_FBM_sub(Environment x,
                   const IntegerVector& row_ind,
                   const IntegerVector& col_ind) {
  
  XPtr<FBM> xptr = x["address"];
  SubBMAcc<double> macc(xptr, row_ind - 1, col_ind - 1);
  
  return sum_mat_like(macc);
}

/*** R
library(bigstatsr)
fbm <- FBM(10e3, 10e3, init = rnorm(1e8))
mat <- fbm[]
system.time(res1 <- sum_mat(mat))
system.time(res2 <- sum_FBM(fbm))
all.equal(res2, res1)
system.time(res3 <- sum_FBM_sub(fbm, rows_along(fbm), cols_along(fbm)))
all.equal(res3, res1)

microbenchmark::microbenchmark(
  sum_mat(mat),
  sum_FBM(fbm),
  sum_FBM_sub(fbm, rows_along(fbm), cols_along(fbm))
)
*/
