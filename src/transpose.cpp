/******************************************************************************/

#include "bigstatsr.h"

/******************************************************************************/

// recursive divide and conquer function
template <typename T>
void transpose3_rec(MatrixAccessor<T> macc,
                    MatrixAccessor<T> macc2,
                    int i_min, int i_max,
                    int j_min, int j_max) {

  if ((j_max - j_min) > 64) {
    int j_mid = (j_min + j_max) / 2;
    transpose3_rec(macc, macc2, i_min, i_max, j_min, j_mid);
    transpose3_rec(macc, macc2, i_min, i_max, j_mid, j_max);
  } else {
    if ((i_max - i_min) > 64) {
      int i_mid = (i_min + i_max) / 2;
      transpose3_rec(macc, macc2, i_min, i_mid, j_min, j_max);
      transpose3_rec(macc, macc2, i_mid, i_max, j_min, j_max);
    } else {
      int i, j;
      for (j = j_min; j < j_max; j++) {
        for (i = i_min; i < i_max; i++) {
          macc[j][i] = macc2[i][j];
        }
      }
    }
  }
}

template <typename T>
void transpose3(MatrixAccessor<T> macc, MatrixAccessor<T> macc2) {
  transpose3_rec(macc, macc2, 0, macc.nrow(), 0, macc.ncol());
}

// Dispatch function for transpose3
// [[Rcpp::export]]
void transpose3(XPtr<BigMatrix> xpMat, XPtr<BigMatrix> xpMat2) {

  switch(xpMat->matrix_type()) {
  case 1:
    return transpose3(MatrixAccessor<char>(*xpMat),
                      MatrixAccessor<char>(*xpMat2));
  case 2:
    return transpose3(MatrixAccessor<short>(*xpMat),
                      MatrixAccessor<short>(*xpMat2));
  case 3:
    return transpose3(MatrixAccessor<unsigned char>(*xpMat),
                      MatrixAccessor<unsigned char>(*xpMat2));
  case 4:
    return transpose3(MatrixAccessor<int>(*xpMat),
                      MatrixAccessor<int>(*xpMat2));
  case 6:
    return transpose3(MatrixAccessor<float>(*xpMat),
                      MatrixAccessor<float>(*xpMat2));
  case 8:
    return transpose3(MatrixAccessor<double>(*xpMat),
                      MatrixAccessor<double>(*xpMat2));
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/
