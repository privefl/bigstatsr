#ifndef BM_ACC_H
#define BM_ACC_H

/******************************************************************************/

#include <bigstatsr/FBM.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

template <typename T>
class BMAcc {
public:
  BMAcc(const FBM * xpBM) {
    _pMat = static_cast<T*>(xpBM->matrix());
    _nrow = xpBM->nrow();
    _ncol = xpBM->ncol();
  }

  inline T& operator()(size_t i, size_t j) {
    return _pMat[i + j * _nrow];
  }

  size_t nrow() const { return _nrow; }
  size_t ncol() const { return _ncol; }

protected:
  T* _pMat;
  size_t _nrow;
  size_t _ncol;
};

/******************************************************************************/

template <typename T>
class SubBMAcc : public BMAcc<T> {
public:
  SubBMAcc(const FBM * xpBM,
           const IntegerVector& row_ind,
           const IntegerVector& col_ind)
    : BMAcc<T>(xpBM) {

      size_t n = row_ind.size();
      std::vector<size_t> row_ind2(n);
      for (size_t i = 0; i < n; i++)
        row_ind2[i] = static_cast<size_t>(row_ind[i]);
      _row_ind = row_ind2;

      size_t m = col_ind.size();
      std::vector<size_t> col_ind2(m);
      for (size_t j = 0; j < m; j++)
        col_ind2[j] = static_cast<size_t>(col_ind[j]);
      _col_ind = col_ind2;
    }

  inline T& operator()(size_t i, size_t j) {
    // https://stackoverflow.com/a/7076312/6103040
    return this->_pMat[_row_ind[i] + _col_ind[j] * this->_nrow];
  }

  size_t nrow() const { return _row_ind.size(); }
  size_t ncol() const { return _col_ind.size(); }

protected:
  std::vector<size_t> _row_ind;
  std::vector<size_t> _col_ind;
};

/******************************************************************************/

template <typename T>
class VecBMAcc : public BMAcc<T> {
public:
  VecBMAcc(const FBM * xpBM,
           const NumericVector& elem_ind)
    : BMAcc<T>(xpBM) {

      size_t n = elem_ind.size();
      std::vector<size_t> elem_ind2(n);
      for (size_t k = 0; k < n; k++)
        elem_ind2[k] = static_cast<size_t>(elem_ind[k]);
      _elem_ind = elem_ind2;
    }

  inline T& operator[](size_t k) {
    // https://stackoverflow.com/a/7076312/6103040
    return this->_pMat[_elem_ind[k]];
  }

  size_t nelem() const { return _elem_ind.size(); }

protected:
  std::vector<size_t> _elem_ind;
};

/******************************************************************************/

#endif // BM_ACC_H
