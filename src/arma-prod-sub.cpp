/******************************************************************************/

#include <bigstatsr/arma-strict-R-headers.h>
#include <bigstatsr/BMAcc-dispatcher.h>

/******************************************************************************/

template <class C>
arma::mat& extract_submat(C macc,
                          arma::mat& to_fill,
                          const std::vector<size_t>& ind_row,
                          const std::vector<size_t>& ind_col) {

  int n = ind_row.size();
  int m = ind_col.size();

  for (int j = 0; j < m; j++)
    for (int i = 0; i < n; i++)
      to_fill(i, j) = macc(ind_row[i], ind_col[j]);

  return to_fill;
}

/******************************************************************************/

template <class C>
arma::mat prod_FBM_block_mat(C macc,
                             const arma::mat& Y,
                             const IntegerVector& rowInd,
                             const IntegerVector& colInd,
                             int max_size) {

  int n = rowInd.size();
  int m = colInd.size();

  arma::mat XY(n, Y.n_cols, arma::fill::zeros);
  arma::mat  X(n, max_size);  // temporary matrix to access blocks

  std::vector<size_t> rows = vec_int_to_size(rowInd, macc.nrow(), 1);
  std::vector<size_t> sub_cols(max_size);

  for (int j = 0; j < m; ) {

    int k;
    for (k = 0; k < max_size && j < m; k++, j++) sub_cols[k] = colInd[j] - 1;

    if (k < max_size) {
      // last block can be shorter
      sub_cols.resize(k);
      XY += extract_submat(macc, X, rows, sub_cols).head_cols(k) * Y.rows(j - k, j - 1);
    } else {
      // k == max_size
      XY += extract_submat(macc, X, rows, sub_cols)              * Y.rows(j - k, j - 1);
    }
  }

  return XY;
}

/******************************************************************************/

#define CALL_PROD_BLOCK(ACC) {                                                 \
  return prod_FBM_block_mat(ACC, Y, rowInd, colInd, max_size);                 \
}

// Dispatch function for prod_FBM_block_mat
// [[Rcpp::export]]
arma::mat prod_FBM_block_mat(Environment BM,
                             const arma::mat& Y,
                             const IntegerVector& rowInd,
                             const IntegerVector& colInd,
                             int max_size) {

  DISPATCH_MATACC(CALL_PROD_BLOCK)
}

/******************************************************************************/

template <class C>
arma::mat cprod_FBM_block_mat(C macc,
                              const arma::mat& Y,
                              const IntegerVector& rowInd,
                              const IntegerVector& colInd,
                              int max_size) {

  int n = rowInd.size();
  int m = colInd.size();

  arma::mat XtY(m, Y.n_cols);  // no need to init
  arma::mat   X(n, max_size);  // temporary matrix to access blocks

  std::vector<size_t> rows = vec_int_to_size(rowInd, macc.nrow(), 1);
  std::vector<size_t> sub_cols(max_size);

  for (int j = 0; j < m; ) {

    int k;
    for (k = 0; k < max_size && j < m; k++, j++) sub_cols[k] = colInd[j] - 1;

    if (k < max_size) {
      // last block can be shorter
      sub_cols.resize(k);
      XtY.rows(j - k, j - 1) =
        extract_submat(macc, X, rows, sub_cols).head_cols(k).t() * Y;
    } else {
      // k == max_size
      XtY.rows(j - k, j - 1) =
        extract_submat(macc, X, rows, sub_cols).t()              * Y;
    }
  }

  return XtY;
}

/******************************************************************************/

#define CALL_CPROD_BLOCK(ACC) {                                                \
  return cprod_FBM_block_mat(ACC, Y, rowInd, colInd, max_size);                \
}

// Dispatch function for prod_FBM_block_mat
// [[Rcpp::export]]
arma::mat cprod_FBM_block_mat(Environment BM,
                              const arma::mat& Y,
                              const IntegerVector& rowInd,
                              const IntegerVector& colInd,
                              int max_size) {

  DISPATCH_MATACC(CALL_CPROD_BLOCK)
}

/******************************************************************************/
