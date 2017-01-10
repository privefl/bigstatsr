#include <RcppArmadillo.h>
#define NDEBUG 1

#include <bigmemory/MatrixAccessor.hpp>

template<typename T>
class SubMatrixAccessor {
public:
  typedef T value_type;

public:
  SubMatrixAccessor(BigMatrix &bm,
                    const IntegerVector &row_ind,
                    const IntegerVector &col_ind) {
    _pMat = reinterpret_cast<T*>(bm.matrix());
    _totalRows = bm.total_rows();
    _row_ind = row_ind;
    _col_ind = col_ind;
    _nrow = row_ind.size();
    _ncol = col_ind.size();
  }

  inline T* operator() (int i, int j) {
    return *(_pMat + _totalRows * _col_ind[j]  + _row_ind[i]);
  }

  int nrow() const {
    return _nrow;
  }

  int ncol() const {
    return _ncol;
  }

protected:
  T *_pMat;
  int _totalRows;
  int _nrow;
  int _ncol;
  IntegerVector _row_ind;
  IntegerVector _col_ind;
};
