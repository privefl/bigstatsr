#ifndef BM_ACC_H
#define BM_ACC_H

/******************************************************************************/

#include <bigstatsr/FBM.h>

using namespace Rcpp;
using std::size_t;

#define NA_FLOAT __FLT_MIN__

/******************************************************************************/

template <typename T>
class BMAcc_RW {
public:
  BMAcc_RW(FBM_RW * xpBM) {
    _pMat = static_cast<T*>(xpBM->matrix());
    _nrow = xpBM->nrow();
    _ncol = xpBM->ncol();
  }

  inline T& operator()(size_t i, size_t j) {
    return _pMat[i + j * _nrow];
  }

  inline T& operator[](size_t k) {
    return _pMat[k];
  }

  size_t nrow() const { return _nrow; }
  size_t ncol() const { return _ncol; }
  size_t size() const { return _nrow * _ncol; }

protected:
  T* _pMat;
  size_t _nrow;
  size_t _ncol;
};

/******************************************************************************/

template <typename T>
class SubBMAcc_RW : public BMAcc_RW<T> {
public:
  SubBMAcc_RW(FBM_RW * xpBM,
              const IntegerVector& row_ind,
              const IntegerVector& col_ind,
              int sub = 0)
    : BMAcc_RW<T>(xpBM) {
      _row_ind = vec_int_to_size(row_ind, xpBM->nrow(), sub);
      _col_ind = vec_int_to_size(col_ind, xpBM->ncol(), sub);
    }

  inline T& operator()(size_t i, size_t j) {
    // https://stackoverflow.com/a/32087373/6103040
    return BMAcc_RW<T>::operator()(_row_ind[i], _col_ind[j]);
  }

  // WARNING: operator[] is not redefined

  size_t nrow() const { return _row_ind.size(); }
  size_t ncol() const { return _col_ind.size(); }

protected:
  std::vector<size_t> _row_ind;
  std::vector<size_t> _col_ind;
};

/******************************************************************************/

template <typename T>
class BMAcc {
public:
  BMAcc(FBM * xpBM) {
    _pMat = static_cast<const T*>(xpBM->matrix());
    _nrow = xpBM->nrow();
    _ncol = xpBM->ncol();
  }

  inline T operator()(size_t i, size_t j) {
    return _pMat[i + j * _nrow];
  }

  inline T operator[](size_t k) {
    return _pMat[k];
  }

  size_t nrow() const { return _nrow; }
  size_t ncol() const { return _ncol; }
  size_t size() const { return _nrow * _ncol; }

protected:
  const T* _pMat;
  size_t _nrow;
  size_t _ncol;
};

/******************************************************************************/

template <typename T>
class SubBMAcc : public BMAcc<T> {
public:
  SubBMAcc(FBM * xpBM,
           const IntegerVector& row_ind,
           const IntegerVector& col_ind,
           int sub = 0)
    : BMAcc<T>(xpBM) {
      _row_ind = vec_int_to_size(row_ind, xpBM->nrow(), sub);
      _col_ind = vec_int_to_size(col_ind, xpBM->ncol(), sub);
    }

  inline T operator()(size_t i, size_t j) {
    // https://stackoverflow.com/a/32087373/6103040
    return BMAcc<T>::operator()(_row_ind[i], _col_ind[j]);
  }

  // WARNING: operator[] is not redefined

  size_t nrow() const { return _row_ind.size(); }
  size_t ncol() const { return _col_ind.size(); }

protected:
  std::vector<size_t> _row_ind;
  std::vector<size_t> _col_ind;
};

/******************************************************************************/

// For biglasso
template<typename T>
class SubMatCovAcc : public SubBMAcc<T> {
public:
  SubMatCovAcc(FBM * xpBM,
               const IntegerVector& row_ind,
               const IntegerVector& col_ind,
               const NumericMatrix& covar,
               int sub = 0)
    : SubBMAcc<T>(xpBM, row_ind, col_ind, sub) {

      _ncolsub = col_ind.size();

      if (covar.nrow() != 0) {
        myassert_size(row_ind.size(), covar.nrow());
        _ncoladd = covar.ncol();
        _covar = covar;
      }  else {
        _ncoladd = 0;
      }
    }

  inline double operator() (size_t i, size_t j) {
    int j2 = j - _ncolsub;
    if (j2 < 0) {
      // https://stackoverflow.com/a/32087373/6103040
      return SubBMAcc<T>::operator()(i, j);
    } else {
      return _covar(i, j2);
    }
  }

  size_t nrow() const { return this->_row_ind.size(); }
  size_t ncol() const { return _ncolsub + _ncoladd; }

protected:
  size_t _ncolsub;
  size_t _ncoladd;
  NumericMatrix _covar;
};

/******************************************************************************/

#endif // BM_ACC_H
