// [[Rcpp::depends(BH, bigstatsr)]]
#include <bigstatsr/BMAcc.h>

// [[Rcpp::export]]
void df2FBM(Environment X, DataFrame x,
            const IntegerVector& ind_row,
            const IntegerVector& ind_col) {

  XPtr<FBM> xptr = X["address"];
  SubBMAcc<double> macc(xptr, ind_row - 1, ind_col - 1);
  NumericVector col;

  for (size_t j = 0; j < macc.ncol(); j++) {
    col = as<NumericVector>(x[j]);
    for (size_t i = 0; i < macc.nrow(); i++) {
      macc(i, j) = col[i];
    }
  }
}

