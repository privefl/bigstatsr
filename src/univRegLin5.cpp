/******************************************************************************/

#include <RcppArmadillo.h>
#include <bigstatsr/SubMatAcc.h>
#include <bigstatsr/univLinReg.hpp>

using namespace Rcpp;

/******************************************************************************/

#define CALL_UNIVLINREG(ACC) return bigstatsr::univLinReg5(ACC, covar_U, y);

// Dispatch function for univLinReg5
// [[Rcpp::export]]
List univLinReg5(const S4& BM,
                 const arma::mat& covar_U,
                 const arma::vec& y,
                 const IntegerVector& rowInd,
                 const IntegerVector& colInd) {

  DISPATCH_SUBMATACC(CALL_UNIVLINREG)
}

/******************************************************************************/
