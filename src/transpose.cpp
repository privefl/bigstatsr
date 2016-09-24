// [[Rcpp::depends(bigmemory, BH)]]

#include <Rcpp.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;



void transpose3_rec(MatrixAccessor<char> macc,
                    MatrixAccessor<char> macc2,
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
      for (int j = j_min; j < j_max; j++) {
        for (int i = i_min; i < i_max; i++) {
          macc[i][j] = macc2[j][i];
        }
      }
    }
  }

  return;
}

// [[Rcpp::export]]
void transpose3(SEXP pBigMat, const SEXP pBigMat2) {
  XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<char> macc(*xpMat);

  XPtr<BigMatrix> xpMat2(pBigMat2);
  MatrixAccessor<char> macc2(*xpMat2);

  transpose3_rec(macc, macc2, 0, xpMat2->nrow(), 0, xpMat2->ncol());

  return;
}
