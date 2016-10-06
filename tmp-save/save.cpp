/******************************************************************************/
// SLOW
// [[Rcpp::export]]
void crossprodCpp(SEXP res, SEXP pBigMat,
                  const IntegerVector& rowInd,
                  const NumericVector& mean,
                  const NumericVector& sd) {

  XPtr<BigMatrix> xpRes(res);
  MatrixAccessor<double> macc2(*xpRes);
  XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<char> macc(*xpMat);

  int n = rowInd.size();
  int m = xpMat->ncol();

  int l;

  for (int j = 0; j < m; j++) {
    if (j % 100 == 0) printf("%d ", j);
    for (int i = 0; i <= j; i++) {
      macc2[j][i] = 0;
      for (int k = 0; k < n; k++) {
        l = rowInd[k] - 1;
        macc2[j][i] += (macc[j][l] - mean[j]) *
          (macc[i][l] - mean[i]) / (sd[j] * sd[i]);
      }
    }
  }

  return;
}

/******************************************************************************/

// doesn't work with a sub.big.matrix
// [[Rcpp::export]]
void crossprodEigen2(SEXP res,
                     const Eigen::Map<Eigen::MatrixXd> X,
                     const Eigen::Map<Eigen::MatrixXd> Y) {

  XPtr<BigMatrix> xpMatRes(res);

  Eigen::Map<Eigen::MatrixXd> bMRes =
    Eigen::Map<Eigen::MatrixXd>((double*)xpMatRes->matrix(),
                                xpMatRes->nrow(),
                                xpMatRes->ncol());

  bMRes = X.transpose() * Y;

  return;
}

/******************************************************************************/

// doesn't work with a sub.big.matrix
// [[Rcpp::export]]
void crossprodEigen3(SEXP res,
                     const Eigen::Map<Eigen::MatrixXd> X,
                     const Eigen::Map<Eigen::MatrixXd> Y) {

  XPtr<BigMatrix> xpMatRes(res);

  Eigen::Map<Eigen::MatrixXd> bMRes =
    Eigen::Map<Eigen::MatrixXd>((double*)xpMatRes->matrix(),
                                xpMatRes->nrow(),
                                xpMatRes->ncol());

  bMRes = X.adjoint() * Y;

  return;
}

/******************************************************************************/
