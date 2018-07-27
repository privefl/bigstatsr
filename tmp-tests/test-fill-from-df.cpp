// [[Rcpp::depends(BH, bigstatsr)]]
#include <bigstatsr/BMAcc.h>

// [[Rcpp::plugins(unwindProtect)]]

// [[Rcpp::export]]
void df2FBM(Environment X, DataFrame x,
            const IntegerVector& ind_row,
            const IntegerVector& ind_col) {

  XPtr<FBM> xptr = X["address"];
  SubBMAcc<double> macc(xptr, ind_row - 1, ind_col - 1);

  size_t i, j, n = macc.nrow(), m = macc.ncol();
  NumericVector col(n);

  for (j = 0; j < m; j++) {
    col = as<NumericVector>(x[j]);
    for (i = 0; i < n; i++) {
      macc(i, j) = col[i];
    }
  }
}
