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
              const IntegerVector& col_ind)
    : BMAcc_RW<T>(xpBM) {

      size_t ind, i, j;

      size_t LIM_N = xpBM->nrow();
      size_t n = row_ind.size();
      std::vector<size_t> row_ind2(n);
      for (i = 0; i < n; i++) {
        ind = static_cast<size_t>(row_ind[i]);
        myassert_bounds(ind, LIM_N);
        row_ind2[i] = ind;
      }
      _row_ind = row_ind2;

      size_t LIM_M = xpBM->ncol();
      size_t m = col_ind.size();
      std::vector<size_t> col_ind2(m);
      for (j = 0; j < m; j++) {
        ind = static_cast<size_t>(col_ind[j]);
        myassert_bounds(ind, LIM_M);
        col_ind2[j] = ind;
      }
      _col_ind = col_ind2;
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
           const IntegerVector& col_ind)
    : BMAcc<T>(xpBM) {

      size_t ind, i, j;

      size_t LIM_N = xpBM->nrow();
      size_t n = row_ind.size();
      std::vector<size_t> row_ind2(n);
      for (i = 0; i < n; i++) {
        ind = static_cast<size_t>(row_ind[i]);
        myassert_bounds(ind, LIM_N);
        row_ind2[i] = ind;
      }
      _row_ind = row_ind2;

      size_t LIM_M = xpBM->ncol();
      size_t m = col_ind.size();
      std::vector<size_t> col_ind2(m);
      for (j = 0; j < m; j++) {
        ind = static_cast<size_t>(col_ind[j]);
        myassert_bounds(ind, LIM_M);
        col_ind2[j] = ind;
      }
      _col_ind = col_ind2;
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

#endif // BM_ACC_H
