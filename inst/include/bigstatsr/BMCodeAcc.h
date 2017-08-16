#ifndef BM_CODE_ACC_H
#define BM_CODE_ACC_H

/******************************************************************************/

#include <bigstatsr/BMAcc.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

class SubBMCode256Acc : public SubBMAcc<unsigned char> {
public:
  SubBMCode256Acc(const FBM * xpBM,
                  const IntegerVector& row_ind,
                  const IntegerVector& col_ind,
                  const NumericVector& code256)
    : SubBMAcc<unsigned char>(xpBM, row_ind, col_ind) {
      _code256 = code256;
    }

  inline double operator()(size_t i, size_t j) {
    // https://stackoverflow.com/a/32087373/6103040
    return _code256[SubBMAcc<unsigned char>::operator()(i, j)];
  }

protected:
  NumericVector _code256;
};

/******************************************************************************/

// template <typename T>
// class VecBMAcc : public BMAcc<T> {
// public:
//   VecBMAcc(const FBM * xpBM,
//            const NumericVector& elem_ind)
//     : BMAcc<T>(xpBM) {
//
//       size_t n = elem_ind.size();
//       std::vector<size_t> elem_ind2(n);
//       for (size_t k = 0; k < n; k++)
//         elem_ind2[k] = static_cast<size_t>(elem_ind[k]);
//       _elem_ind = elem_ind2;
//     }
//
//   inline T& operator[](size_t k) {
//     // https://stackoverflow.com/a/7076312/6103040
//     return this->_pMat[_elem_ind[k]];
//   }
//
//   size_t nelem() const { return _elem_ind.size(); }
//
// protected:
//   std::vector<size_t> _elem_ind;
// };

/******************************************************************************/

#endif // BM_CODE_ACC_H
