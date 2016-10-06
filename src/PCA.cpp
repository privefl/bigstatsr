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

// [[Rcpp::export]]
Eigen::MatrixXd crossprodEigen5(const Eigen::Map<Eigen::MatrixXd> X,
                                const Eigen::Map<Eigen::MatrixXd> Y) {
  return X.adjoint() * Y;
}

/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix& scaling(NumericMatrix& source,
                       const NumericVector& mean,
                       const NumericVector& sd) {
  int n = source.rows();
  int m = source.cols();

  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      source(i,j) -= mean[j];
      source(i,j) /= sd[j];
    }
  }

  return(source);
}

/******************************************************************************/

// [[Rcpp::export]]
void incrSup(SEXP pBigMat, const NumericMatrix& source) {
  XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<double> macc(*xpMat);

  for (int j = 0; j < xpMat->ncol(); j++) {
    for (int i = 0; i <= j; i++) {
      macc[j][i] += source(i,j);
    }
  }

  return;
}

/******************************************************************************/

// [[Rcpp::export]]
void incrAll(SEXP pBigMat, const NumericMatrix& source) {
  XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<double> macc(*xpMat);

  for (int j = 0; j < xpMat->ncol(); j++) {
    for (int i = 0; i < xpMat->nrow(); i++) {
      macc[j][i] += source(i,j);
    }
  }

  return;
}

/******************************************************************************/

// [[Rcpp::export]]
void complete(SEXP pBigMat) {
  XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<double> macc(*xpMat);

  for (int j = 0; j < xpMat->ncol()-1; j++) {
    for (int i = j+1; i < xpMat->nrow(); i++) {
      macc[j][i] = macc[i][j];
    }
  }

  return;
}

/******************************************************************************/
