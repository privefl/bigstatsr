/******************************************************************************/

#include <bigstatsr/arma-strict-R-headers.h>
#include <bigstatsr/BMAcc-dispatcher.h>

/******************************************************************************/

inline arma::mat FBM_RW2arma(Rcpp::Environment BM) {

  Rcpp::XPtr<FBM_RW> xpBM = BM["address_rw"];
  myassert(xpBM->matrix_type() == 8,
           "Mapping to arma::mat is available for 'double' FBMs only.");

  return arma::mat((double*)xpBM->matrix(), xpBM->nrow(), xpBM->ncol(), false);
}

/******************************************************************************/

template <class C>
arma::mat& _extract_scaled_submat(C macc,
                                  arma::mat& to_fill,
                                  const IntegerVector& rowInd,
                                  const IntegerVector& colInd,
                                  const NumericVector& center,
                                  const NumericVector& scale) {

  std::vector<size_t> rows = vec_int_to_size(rowInd, macc.nrow(), 1);
  std::vector<size_t> cols = vec_int_to_size(colInd, macc.ncol(), 1);

  int n = rowInd.size();
  int m = colInd.size();

  for (int j = 0; j < m; j++)
    for (int i = 0; i < n; i++)
      to_fill(i, j) = (macc(rows[i], cols[j]) - center[j]) / scale[j];

  // fill the rest with 0s (should be 1 column max)
  int m2 = to_fill.n_cols;
  if (m2 > m) {
    myassert(m2 == (m + 1), ERROR_BUG);
    for (int i = 0; i < n; i++) to_fill(i, m) = 0;
  }

  return to_fill;
}

/******************************************************************************/

template <class C>
void increment_scaled_tcrossprod(arma::mat& K,
                                 arma::mat& part_temp,
                                 C macc,
                                 const IntegerVector& rowInd,
                                 const IntegerVector& colInd,
                                 const NumericVector& center,
                                 const NumericVector& scale) {

  part_temp = _extract_scaled_submat(macc, part_temp, rowInd, colInd, center, scale);
  K += part_temp * part_temp.t();
}

/******************************************************************************/

#define CALL_INCR_TCPROD(ACC) {                                                \
  return increment_scaled_tcrossprod(armaK, part_temp, ACC,                    \
                                     rowInd, colInd, center, scale);           \
}

// Dispatch function for prod_FBM_block_mat
// [[Rcpp::export]]
void increment_scaled_tcrossprod(Environment K,
                                 arma::mat& part_temp,
                                 Environment BM,
                                 const IntegerVector& rowInd,
                                 const IntegerVector& colInd,
                                 const NumericVector& center,
                                 const NumericVector& scale) {

  arma::mat armaK = FBM_RW2arma(K);

  DISPATCH_MATACC(CALL_INCR_TCPROD)
}

/******************************************************************************/
