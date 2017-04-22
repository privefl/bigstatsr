/******************************************************************************/

// [[Rcpp::depends(bigmemory, BH, RcppArmadillo)]]
#include <RcppArmadillo.h>
#define NDEBUG 1 // https://github.com/RcppCore/RcppArmadillo/issues/116

using namespace Rcpp;

#include <bigmemory/MatrixAccessor.hpp>

/******************************************************************************/

#ifndef UTILS_H
#define UTILS_H

// also defined in R/utils.R
const char* const ERROR_TYPE = "unknown type detected for big.matrix object!";
const char* const ERROR_DIM = "incompatibility between dimensions";
const char* const ERROR_SUB =
  "you can't use this function on a sub.big.matrix!";

inline void myassert(bool cond, const char *msg) {
  if (!cond) throw Rcpp::exception(msg);
}

#endif // UTILS_H

/******************************************************************************/

template<typename T>
class SubMatAcc {
public:
  SubMatAcc(BigMatrix &bm,
            const IntegerVector &row_ind,
            const IntegerVector &col_ind) {
    if (bm.is_submatrix()) throw Rcpp::exception(ERROR_SUB);

    int n = row_ind.size();
    std::vector<index_type> row_ind2(n);
    for (int i = 0; i < n; i++)
      row_ind2[i] = static_cast<index_type>(row_ind[i]);

    int m = col_ind.size();
    std::vector<index_type> col_ind2(m);
    for (int j = 0; j < m; j++)
      col_ind2[j] = static_cast<index_type>(col_ind[j]);

    _pMat = reinterpret_cast<T*>(bm.matrix());
    _totalRows = bm.total_rows();
    _row_ind = row_ind2;
    _col_ind = col_ind2;
    _nrow = row_ind.size();
    _ncol = col_ind.size();
  }

  inline T operator() (int i, int j) {
    return *(_pMat + _totalRows * _col_ind[j] + _row_ind[i]);
  }

  int nrow() const {
    return _nrow;
  }

  int ncol() const {
    return _ncol;
  }

protected:
  T *_pMat;
  index_type _totalRows;
  int _nrow;
  int _ncol;
  std::vector<index_type> _row_ind;
  std::vector<index_type> _col_ind;
};

/******************************************************************************/

class RawSubMatAcc : public SubMatAcc<unsigned char> {
public:
  RawSubMatAcc(BigMatrix& bm,
               const IntegerVector& row_ind,
               const IntegerVector& col_ind,
               const NumericVector& lookup)
    : SubMatAcc<unsigned char>(bm, row_ind, col_ind) {
      _lookup = lookup;
    }

  inline double operator() (int i, int j) {
    return _lookup[*(_pMat + _totalRows * _col_ind[j] + _row_ind[i])];
  }

protected:
  NumericVector _lookup;
};

/******************************************************************************/

// For biglasso
template<typename T>
class SubMatCovAcc {
public:
  SubMatCovAcc(BigMatrix& bm,
               const IntegerVector& row_ind,
               const NumericMatrix& covar) {
    if (bm.is_submatrix()) throw Rcpp::exception(ERROR_SUB);

    if (covar.nrow() != 0) {
      myassert(row_ind.length() == covar.nrow(), ERROR_DIM);
      _ncoladd = covar.ncol();
      _covar = covar;
    }  else {
      _ncoladd = 0;
    }

    int n = row_ind.size();
    std::vector<index_type> row_ind2(n);
    for (int i = 0; i < n; i++)
      row_ind2[i] = static_cast<index_type>(row_ind[i]);

    _pMat = reinterpret_cast<T*>(bm.matrix());
    _totalRows = bm.total_rows();
    _row_ind = row_ind2;
    _nrow = row_ind.size();
    _ncolBM = bm.ncol();
  }

  inline double operator() (int i, int j) {
    if (j < _ncolBM) {
      return *(_pMat + _totalRows * j + _row_ind[i]);
    } else {
      return _covar(i, j - _ncolBM);
    }
  }

  int nrow() const {
    return _nrow;
  }

  int ncol() const {
    return _ncolBM + _ncoladd;
  }

protected:
  T *_pMat;
  index_type _totalRows;
  int _nrow;
  int _ncolBM;
  int _ncoladd;
  std::vector<index_type> _row_ind;
  NumericMatrix _covar;
};

/******************************************************************************/

class RawSubMatCovAcc : public SubMatCovAcc<unsigned char> {
public:
  RawSubMatCovAcc(BigMatrix& bm,
                  const IntegerVector& row_ind,
                  const NumericMatrix& covar,
                  const NumericVector& lookup)
    : SubMatCovAcc<unsigned char>(bm, row_ind, covar) {
      _lookup = lookup;
    }

  inline double operator() (int i, int j) {
    if (j < _ncolBM) {
      return _lookup[*(_pMat + _totalRows * j + _row_ind[i])];
    } else {
      return _covar(i, j - _ncolBM);
    }
  }

protected:
  NumericVector _lookup;
};

/******************************************************************************/

// For sparseSVM
template<typename T>
class SubIntMatCovAcc : public SubMatCovAcc<T> {
public:
  SubIntMatCovAcc(BigMatrix& bm,
                  const IntegerVector& row_ind,
                  const NumericMatrix& covar)
    : SubMatCovAcc<T>(bm, row_ind, covar) {}

  inline double operator() (int i, int j) {
    return j == 0 ? 1.0 : SubMatCovAcc<T>::operator()(i, j-1);
  }

  int ncol() const {
    return 1 + SubMatCovAcc<T>::ncol();
  }
};

/******************************************************************************/

class RawSubIntMatCovAcc : public RawSubMatCovAcc {
public:
  RawSubIntMatCovAcc(BigMatrix& bm,
                     const IntegerVector& row_ind,
                     const NumericMatrix& covar,
                     const NumericVector& lookup)
    : RawSubMatCovAcc(bm, row_ind, covar, lookup) {}

  inline double operator() (int i, int j) {
    return j == 0 ? 1.0 : RawSubMatCovAcc::operator()(i, j-1);
  }

  int ncol() const {
    return 1 + RawSubMatCovAcc::ncol();
  }
};

/******************************************************************************/
