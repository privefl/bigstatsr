// [[Rcpp::depends(bigstatsr)]]
#include <bigstatsr/biglasso/logistic.hpp>
#include <bigstatsr/SubMatCovAcc.h>

using namespace Rcpp;


/******************************************************************************/

class RawSubTripleCovAcc : public RawSubMatCovAcc {
public:
  RawSubTripleCovAcc(BigMatrix& bm,
                     const IntegerVector& row_ind,
                     const NumericMatrix& covar,
                     const NumericVector& lookup)
    : RawSubMatCovAcc(bm, row_ind, covar, lookup) {
    NumericMatrix tmp(lookup.size(), 3);
    tmp(_, 0) = lookup;
    tmp(_, 1) = lookup >= 0.5;
    tmp(_, 2) = lookup > 1.5;
    _lookup3 = tmp;
    _ncolBM = 3 * bm.ncol();
  }

  inline double operator() (int i, int j) {
    if (j < _ncolBM) {
      int j2 = j / 3;
      return _lookup3(*(_pMat + _totalRows * j2 + _row_ind[i]), j % 3);
    } else {
      return _covar(i, j - _ncolBM);
    }
  }

protected:
  NumericMatrix _lookup3;
};

// [[Rcpp::export]]
List COPY_cdfit_binomial_hsr(const S4& BM,
                             const NumericVector& y,
                             const IntegerVector& row_idx,
                             const NumericMatrix& covar,
                             NumericVector& lambda,
                             int L,
                             int lam_scale,
                             double lambda_min,
                             double alpha,
                             bool user,
                             double eps,
                             int max_iter,
                             const NumericVector& m,
                             int dfmax,
                             bool warn,
                             bool verbose) {

  XPtr<BigMatrix> xpMat = BM.slot("address");

  return  bigstatsr::biglassoLog::COPY_cdfit_binomial_hsr(
    RawSubTripleCovAcc(*xpMat, row_idx, covar, BM.slot("code")),
    y, lambda, L, lam_scale, lambda_min,
    alpha, user, eps, max_iter, m,
    dfmax, warn, verbose);
}

/*** R
source("../R/biglasso.R")
*/
