// [[Rcpp::depends(RcppArmadillo, bigmemory, BH)]]
#include <RcppArmadillo.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;


/******************************************************************************/

// [[Rcpp::export]]
List eigenArma(XPtr<BigMatrix> xpA) {

  arma::mat Am((double*)xpA->matrix(), xpA->nrow(), xpA->ncol(), false);

  arma::vec eigval;
  arma::mat eigvec;

  arma::eig_sym(eigval, eigvec, Am);

  return List::create(_["values"] = eigval,
                      _["vectors"] = eigvec);
}

/******************************************************************************/
