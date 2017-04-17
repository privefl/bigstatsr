/******************************************************************************/

#include "bigstatsr.h"

/******************************************************************************/

// [[Rcpp::export]]
IntegerVector standardize(XPtr<BigMatrix> xpMat, double sd_thr) {

  MatrixAccessor<double> macc(*xpMat);
  int n = macc.nrow();
  int m = macc.ncol();

  double x, xSum, xxSum, m_j, sd_j;
  int i, j;

  vector<int> col_idx;

  for (j = 0; j < m; j++) {

    // compute column mean and standard deviation
    xSum = xxSum = 0;
    for (i = 0; i < n; i++) {
      x = macc[j][i];
      xSum += x;
      xxSum += x*x;
    }
    m_j = xSum / n;
    sd_j = sqrt((xxSum - xSum * xSum / n) / (n-1));

    // standardize
    if (sd_j > sd_thr) {
      for (i = 0; i < n; i++) {
        macc[j][i] -= m_j;
        macc[j][i] /= sd_j;
      }
    } else { // return indices to delete
      col_idx.push_back(j);
    }
  }

  return as<IntegerVector>(Rcpp::wrap(col_idx)) + 1;
}

/******************************************************************************/
