/******************************************************************************/

#include <bigstatsr/univLogReg.hpp>
#include <bigstatsr/BMAcc-dispatcher.h>

using namespace Rcpp;

/******************************************************************************/

#define CALL_IRLS(ACC) {                                                       \
  return bigstatsr::IRLS(ACC, covar, y, z0, w0, tol, maxiter);                 \
}

// Dispatch function for IRLS
// [[Rcpp::export]]
List IRLS(Environment BM,
          arma::mat& covar,
          const arma::vec& y,
          const arma::vec& z0,
          const arma::vec& w0,
          const IntegerVector& rowInd,
          const IntegerVector& colInd,
          double tol,
          int maxiter) {

  DISPATCH_SUBMATACC(CALL_IRLS)
}

/******************************************************************************/
