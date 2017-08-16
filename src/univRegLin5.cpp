/******************************************************************************/

#include <RcppArmadillo.h>
#include <bigstatsr/BMCodeAcc.h>
#include <bigstatsr/univLinReg.hpp>

using namespace Rcpp;

/******************************************************************************/

#define CALL_UNIVLINREG(ACC) return bigstatsr::univLinReg5(ACC, covar_U, y);

// Dispatch function for univLinReg5
// [[Rcpp::export]]
List univLinReg5(Environment FBM,
                 const arma::mat& covar_U,
                 const arma::vec& y,
                 const IntegerVector& rowInd,
                 const IntegerVector& colInd) {

  DISPATCH_SUBMATACC(CALL_UNIVLINREG)
}

/******************************************************************************/
