/******************************************************************************/

#include <bigstatsr/univLinReg.hpp>
#include <bigstatsr/BMAcc-dispatcher.h>

using namespace Rcpp;

/******************************************************************************/

#define CALL_UNIVLINREG(ACC) return bigstatsr::univLinReg5(ACC, covar_U, y, ncores);

// Dispatch function for univLinReg5
// [[Rcpp::export]]
List univLinReg5(Environment BM,
                 const arma::mat& covar_U,
                 const arma::vec& y,
                 const IntegerVector& rowInd,
                 const IntegerVector& colInd,
                 int ncores) {

  DISPATCH_SUBMATACC(CALL_UNIVLINREG)
}

/******************************************************************************/
