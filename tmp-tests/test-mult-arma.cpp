// [[Rcpp::depends(RcppArmadillo)]]

#define STRICT_R_HEADERS

#include <RcppArmadillo.h>
#include <bigstatsr/BMAcc.h>
using namespace Rcpp;


// [[Rcpp::export]]
arma::mat mult_int_dbl(const arma::Mat<int>& x, const arma::mat& y) {
  return x * y;
}

// [[Rcpp::export]]
arma::mat mult_raw_dbl(const arma::Mat<unsigned char>& x, const arma::mat& y) {
  return x * y;
}

// [[Rcpp::export]]
arma::mat mult_sub_int_dbl(Environment fbm,
                           const arma::mat& y,
                           const IntegerVector& rowInd,
                           const IntegerVector& colInd) {

  XPtr<FBM> xpBM = fbm["address"];
  BMAcc<int> macc(xpBM);

  std::vector<size_t> ind_row = vec_int_to_size(rowInd, macc.nrow(), 1);
  std::vector<size_t> ind_col = vec_int_to_size(colInd, macc.ncol(), 1);

  arma::Mat<int> x(ind_row.size(), colInd.size());
  macc.extract_submat(x, ind_row, ind_col);

  return x * y;
}

// [[Rcpp::export]]
arma::mat mult_sub_int_dbl2(Environment fbm,
                            const arma::mat& y,
                            const IntegerVector& rowInd,
                            const IntegerVector& colInd) {

  XPtr<FBM> xpBM = fbm["address"];
  BMAcc<int> macc(xpBM);

  std::vector<size_t> ind_row = vec_int_to_size(rowInd, macc.nrow(), 1);
  std::vector<size_t> ind_col = vec_int_to_size(colInd, macc.ncol(), 1);

  arma::mat x(ind_row.size(), colInd.size());
  macc.extract_submat(x, ind_row, ind_col);

  return x * y;
}

arma::uvec vec_int_to_arma(const IntegerVector& vec_ind) {
  int n = vec_ind.size();
  arma::uvec res(n);
  for (int i = 0; i < n; i++) res[i] = vec_ind[i] - 1;
  return res;
}

// [[Rcpp::export]]
arma::mat mult_sub_int_dbl_arma(Environment fbm,
                                const arma::mat& y,
                                const IntegerVector& rowInd,
                                const IntegerVector& colInd) {

  XPtr<FBM> xpBM = fbm["address"];
  arma::Mat<int> x((int*)xpBM->matrix(), xpBM->nrow(), xpBM->ncol(), false);

  return x.submat(vec_int_to_arma(rowInd), vec_int_to_arma(colInd)) * y;
}

// [[Rcpp::export]]
arma::mat mult_sub_int_dbl3(Environment fbm,
                            const arma::mat& y,
                            const IntegerVector& rowInd,
                            const IntegerVector& colInd,
                            int max_size) {

  XPtr<FBM> xpBM = fbm["address"];
  BMAcc<int> macc(xpBM);

  int n = rowInd.size();
  int m = colInd.size();

  arma::mat prod(n, y.n_cols, arma::fill::zeros);
  arma::mat x(n, max_size);
  std::vector<size_t> rows = vec_int_to_size(rowInd, macc.nrow(), 1);
  std::vector<size_t> sub_cols(max_size);

  for (int j = 0; j < m; ) {

    int k;
    for (k = 0; k < max_size && j < m; k++, j++) sub_cols[k] = colInd[j] - 1;

    if (k < max_size) {  // last block might be shorter
      sub_cols.resize(k);
      macc.extract_submat(x, rows, sub_cols);
      prod += x.head_cols(k) * y.rows(j - k, j - 1);
    } else {
      macc.extract_submat(x, rows, sub_cols);
      prod += x * y.rows(j - max_size, j - 1);
    }
  }

  return prod;
}


/*** R
A <- matrix(1:4, 2)
B <- matrix(rnorm(20), 2)
mult_int_dbl(A, B)
A %*% B

C <- A; storage.mode(C) <- "raw"
mult_raw_dbl(C, B)
# C %*% B  # error

X <- bigstatsr::FBM(10, 20, type = "integer", init = 1:200)
mult_sub_int_dbl(X, B, 1:5, 1:2)
mult_sub_int_dbl2(X, B, 1:5, 1:2)
mult_sub_int_dbl3(X, B, 1:5, 1:2, 1)
X[1:5, 1:2] %*% B
mult_sub_int_dbl_arma(X, B, 1:5, 1:2)
*/
