/******************************************************************************/

#include "bigstatsr.h"

/******************************************************************************/

// [[Rcpp::export]]
void addInter(XPtr<BigMatrix> xpMat2,
              XPtr<BigMatrix> xpMat,
              const IntegerMatrix& arr_ind) {

  MatrixAccessor<double> macc2(*xpMat2);
  int n2 = macc2.nrow();
  int m2 = macc2.ncol();
  MatrixAccessor<double> macc(*xpMat);
  int n = macc.nrow();
  int m = macc.ncol();

  myassert(n == n2, ERROR_DIM);

  int i, j, j2, k, k1, k2;

  // rewrite initial `big.matrix`
  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      macc2[j][i] = macc[j][i];
    }
  }

  // add interactions
  for (k = 0, j2 = m; j2 < m2; k++, j2++) {
    k1 = arr_ind(k, 0) - 1;
    k2 = arr_ind(k, 1) - 1;
    for (i = 0; i < n; i++) {
      macc2[j2][i] = macc[k1][i] * macc[k2][i];
    }
  }
}

/******************************************************************************/
