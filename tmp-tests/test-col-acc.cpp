#include <bigstatsr/BMAcc.h>

template <typename T>
class ColAcc {
public:
  ColAcc(T* pMat, size_t j, size_t nrow) {
    _pMat = pMat + j * nrow;
    _nrow = nrow;
  }

  inline T& operator[](size_t i) {
    return _pMat[i];
  }

  size_t size() const { return _nrow; }

protected:
  T* _pMat;
  size_t _nrow;
};

template <typename T>
class BMAcc2 {
public:
  BMAcc2(const FBM * xpBM) {
    _pMat = static_cast<T*>(xpBM->matrix());
    _nrow = xpBM->nrow();
    _ncol = xpBM->ncol();
  }

  inline T& operator()(size_t i, size_t j) {
    return _pMat[i + j * _nrow];
  }

  ColAcc<T> operator[](size_t j) {
    ColAcc<T> col(_pMat, j, _nrow);
    return col;
  }

  size_t nrow() const { return _nrow; }
  size_t ncol() const { return _ncol; }

protected:
  T* _pMat;
  size_t _nrow;
  size_t _ncol;
};


// [[Rcpp::export]]
void col_acc(Environment X, DataFrame x) {

  XPtr<FBM> xptr = X["address"];
  BMAcc2<double> macc(xptr);
  size_t i, j, n = macc.nrow(), m = macc.ncol();

  NumericVector col_df;
  NumericMatrix res(n, m);
  for (j = 0; j < m; j++) {
    ColAcc<double> col_j = macc[j];
    col_df = as<NumericVector>(x[j]);
    for (i = 0; i < n; i++) {
      col_j[i] = col_df[i];
    }
  }
}

/*** R
library(bigstatsr)
mini_X <- FBM(nrow(iris), 4, init = 0);
col_acc(mini_X, iris[-5])
mini_X[1:6, ]
*/
