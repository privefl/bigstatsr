// [[Rcpp::depends(RcppEigen, BH, bigmemory)]]
#include <RcppEigen.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;


/******************************************************************************/

// [[Rcpp::export]]
void tcrossprodEigen(SEXP res, const Eigen::Map<Eigen::MatrixXd> bM) {

  XPtr<BigMatrix> xpMatRes(res);

  int n = bM.rows();

  Eigen::Map<Eigen::MatrixXd> bMRes =
    Eigen::Map<Eigen::MatrixXd>((double*)xpMatRes->matrix(), n, n);

  bMRes.selfadjointView<Eigen::Upper>().rankUpdate(bM);

  return;
}

/******************************************************************************/

// [[Rcpp::export]]
void tcrossprodEigen2(SEXP res,
                      const Eigen::Map<Eigen::MatrixXd> X,
                      const Eigen::Map<Eigen::MatrixXd> Y) {

  XPtr<BigMatrix> xpMatRes(res);

  Eigen::Map<Eigen::MatrixXd> bMRes =
    Eigen::Map<Eigen::MatrixXd>((double*)xpMatRes->matrix(),
                                xpMatRes->nrow(),
                                xpMatRes->ncol());

  bMRes += X * Y.transpose();

  return;
}

/******************************************************************************/
